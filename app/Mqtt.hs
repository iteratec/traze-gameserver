module Mqtt where

import GameTypes
import Instance
import Config
import Output

import qualified Data.ByteString as BS

import Control.Concurrent
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM
import Data.Aeson
import Data.UUID
import Data.List.Split

import Text.Read

import Data.ByteString.Lazy (toStrict, fromStrict)

import qualified Network.Mosquitto as M
import Network.Mosquitto.Internal.Types

data MqttMessage = MqttMessage String BS.ByteString

data MessageType
    = Steering InstanceName PlayerId
    | Bail InstanceName PlayerId
    | Join InstanceName

-- | publish 
mqttThread :: TQueue (String, BS.ByteString) -> TQueue Interaction -> Config -> IO ()
mqttThread messageQueue commandQueue config = M.withMosquittoLibrary $ do
    m <- M.newMosquitto True (clientName config) (Just ())
    M.setTls m "" $ Just ("", "")
    _ <- M.setUsernamePassword m $ Just (brokerUser config, brokerPassword config)
    M.setTlsInsecure m True
    -- M.onLog m $ const putStrLn
    _ <- M.setReconnectDelay m True 2 30
    M.onMessage m (atomically . (handleMessage commandQueue))
    M.onLog m $ const putStrLn
    M.onConnect m $ \c -> do
        putStrLn "connected to broker"
        print c
        M.subscribe m 0 "traze/+/+/steer"
        M.subscribe m 0 "traze/+/+/bail"
        M.subscribe m 0 "traze/+/join"

    M.onDisconnect m print
    M.onSubscribe m $ curry print
    _ <- M.connect m (brokerHost config) (brokerPort config) 1200

    _ <- forkIO $ forever $ do
        (top, message) <- atomically $ readTQueue messageQueue
        M.publish m False 0 top message

    M.loopForever m
    M.destroyMosquitto m
    return ()

castTickThread :: TQueue Tick -> TQueue (String, BS.ByteString) -> STM ()
castTickThread input output = do
    tick <- readTQueue input
    let message = (\g -> ("traze/1/ticker\0", toStrict $ encode g)) tick
    writeTQueue output message

castGameInstancesThread :: TQueue Instance -> TQueue (String, BS.ByteString) -> STM ()
castGameInstancesThread input output = do
    inst <- peekTQueue input
    writeTQueue output ("traze/games", toStrict $ encode $ [(InstancesOutput (unName inst) (length $ unPlayer inst))])
    
castInstanceThread :: TQueue Instance -> TQueue (String, BS.ByteString) -> STM ()
castInstanceThread input output = do
    inst <- readTQueue input 
    let message = (\i -> ("traze/1/grid\0", toStrict $ encode $ gridToGameState $ unGrid i)) inst
    let playerMessage = (\i -> ("traze/1/players\0", toStrict $ encode $ instanceToPlayersOutput $ i)) inst
    writeTQueue output message
    writeTQueue output playerMessage

castNewPlayerThread :: TQueue Player -> TQueue (String, BS.ByteString) -> STM ()
castNewPlayerThread input output = do
    newP <- readTQueue input
    let message = (\p -> ("traze/1/player/"++ (unMqttClientName p) ++ "\0", toStrict $ encode $ playerToAcceptJoinRequestOutput p)) newP
    writeTQueue output message

handleMessage :: TQueue Interaction -> Message -> STM ()
handleMessage queue (Message _ top payl _ _) = case parseTopic top of
    (Just (Steering _ pid)) -> writeSteerCommand queue pid $ parseSteerInput payl
    (Just (Bail _ pid)) -> writeBailCommand queue pid $ parseBailInput payl
    (Just (Join _)) -> writeJoinCommand queue $ parseJoinInput payl
    _ -> return ()

writeSteerCommand :: TQueue Interaction -> PlayerId -> Maybe SteerInput -> STM ()
writeSteerCommand _ _ Nothing = return ()
writeSteerCommand queue pid (Just stin) = case uuid of
    Just session ->  writeTQueue queue $ GridCommand (MoveCommand pid (Steer $ stInCourse stin)) session
    Nothing -> return ()
    where uuid = (Data.UUID.fromString $ stInPlayerToken stin)

writeBailCommand :: TQueue Interaction -> PlayerId -> Maybe BailInput -> STM ()
writeBailCommand _ _ Nothing = return ()
writeBailCommand queue pid (Just input) = case uuid of
    Just session -> writeTQueue queue $ GridCommand (Quit pid) session
    Nothing -> return ()
    where uuid = (Data.UUID.fromString $ bailPlayerToken input)

writeJoinCommand :: TQueue Interaction -> Maybe JoinInput -> STM()
writeJoinCommand _ Nothing = return ()
writeJoinCommand queue (Just joinInput) = writeTQueue queue (JoinRequest (joInName joinInput) (joInMqttClientName joinInput))
    
parseTopic :: String -> Maybe MessageType
parseTopic top = case (splitOn "/" top) of
     ("traze" : instN : pid : "steer" : []) -> getSteering instN (readMaybe pid)
     ("traze" : instN : pid : "bail"  : []) -> getBail instN (read pid)
     ("traze" : instN : "join" : []) -> Just $ Join instN
     _ -> Nothing

     where getSteering instN (Just pid) = Just $ Steering instN pid
           getSteering _ Nothing = Nothing

           getBail instN (Just pid) = Just $ Bail instN pid
           getBail _ Nothing = Nothing

parseSteerInput :: BS.ByteString -> Maybe SteerInput
parseSteerInput bs = decode $ fromStrict bs

parseBailInput :: BS.ByteString -> Maybe BailInput
parseBailInput bs = decode $ fromStrict bs

parseJoinInput :: BS.ByteString -> Maybe JoinInput
parseJoinInput bs = decode $ fromStrict bs

