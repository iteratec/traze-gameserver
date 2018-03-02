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
import Data.List.Split

import Data.ByteString.Lazy (toStrict, fromStrict)

import qualified Network.Mosquitto as M
import Network.Mosquitto.Internal.Types

data MqttMessage = MqttMessage String BS.ByteString

data MessageType
    = Steering InstanceName PlayerId
    | Bail InstanceName PlayerId
    | Join InstanceName

-- | publish
mqttThread :: TQueue (String, BS.ByteString) -> TQueue Command -> Config -> IO ()
mqttThread gridQueue commandQueue config = M.withMosquittoLibrary $ do
    m <- M.newMosquitto True (clientName config) (Just ())
    M.setTls m "" "" ""
    M.setTlsInsecure m True
    _ <- M.setReconnectDelay m True 2 30
    M.onMessage m (atomically . (handleMessage commandQueue))
    M.onLog m $ const putStrLn
    M.onConnect m $ \c -> do
        putStrLn "connected to broker"
        print c
        M.subscribe m 0 "traze/+/+/steer"
        M.subscribe m 0 "traze/+/+/bail"

    M.onDisconnect m print
    M.onSubscribe m $ curry print
    _ <- M.connect m (brokerHost config) (brokerPort config) 1200

    _ <- forkIO $ forever $ do
        (top, message) <- atomically $ readTQueue gridQueue
        M.publish m False 0 top message

    M.loopForever m
    M.destroyMosquitto m
    return ()

castGridThread :: TQueue Grid -> TQueue (String, BS.ByteString) -> STM ()
castGridThread input output = do
    grid <- readTQueue input
    let message = (\g -> ("traze/1/grid\0", toStrict $ encode $ gridToGameState g)) grid
    writeTQueue output message

castTickThread :: TQueue Tick -> TQueue (String, BS.ByteString) -> STM ()
castTickThread input output = do
    tick <- readTQueue input
    let message = (\g -> ("traze/1/ticker\0", toStrict $ encode g)) tick
    writeTQueue output message

handleMessage :: TQueue Command -> Message -> STM ()
handleMessage queue (Message _ top payl _ _) = case parseTopic top of
    (Just (Steering _ pid)) -> writeSteerCommand queue pid $ parseSteerInput payl
    (Just (Bail _ pid)) -> writeBailCommand queue pid $ parseBailInput payl
    _ -> return ()

writeSteerCommand :: TQueue Command -> PlayerId -> Maybe SteerInput -> STM ()
writeSteerCommand _ _ Nothing = return ()
writeSteerCommand queue pid (Just stin) = writeTQueue queue (MoveCommand pid (Steer $ stInCourse stin))

writeBailCommand :: TQueue Command -> PlayerId -> Maybe BailInput -> STM ()
writeBailCommand _ _ Nothing = return ()
writeBailCommand queue pid (Just _) = writeTQueue queue (Quit pid)

parseTopic :: String -> Maybe MessageType
parseTopic top = case (splitOn "/" top) of
     ("traze" : instName : pid : "steer" : []) -> Just $ Steering instName (read pid)
     ("traze" : instName : pid : "bail"  : []) -> Just $ Bail instName (read pid)
     ("traze" : instName : "join" : []) -> Just $ Join instName
     _ -> Nothing

parseSteerInput :: BS.ByteString -> Maybe SteerInput
parseSteerInput bs = decode $ fromStrict bs

parseBailInput :: BS.ByteString -> Maybe BailInput
parseBailInput bs = decode $ fromStrict bs

castTickerThread :: TQueue Death -> IO ()
castTickerThread = undefined

castPlayersThread :: TQueue Instance -> IO ()
castPlayersThread = undefined

-- connectToBroker :: Config ->

-- | broadcasts the current game state over MQTT
castGameState :: Grid -- the current game state
              -> IO ()
castGameState = undefined

-- | broadcasts the current list of players playing on a given instance
castPlayers :: Instance
            -> IO ()
castPlayers = undefined

castTicker :: Death
           -> IO ()
castTicker = undefined
