{-# LANGUAGE RecordWildCards #-}

module Mqtt where

import InstanceTypes
import Config
import Output
import MqttStructure

import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.STM

import Data.Aeson
import Data.UUID
import Data.ByteString.Lazy (toStrict)

import System.Random

import qualified Network.Mosquitto as M
import Network.Mosquitto.Internal.Types  

-- | publish 
mqttThread :: TQueue MqttMessage -> TQueue Interaction -> Config -> IO ()
mqttThread messageQueue commandQueue config = M.withMosquittoLibrary $ do
    mqttClientName <- randomIO
    m <- M.newMosquitto True (toString mqttClientName) (Just ())
    M.setTls m "" $ Just ("", "")
    _ <- M.setUsernamePassword m $ credentialsToTuple $ brokerCredentials config
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
        (MqttMessage top message retain) <- atomically $ readTQueue messageQueue
        M.publish m retain 0 top message

    M.loopForever m
    M.destroyMosquitto m
    return ()

handleMessage :: TQueue Interaction -> Message -> STM ()
handleMessage queue (Message _ top payl _ _) = case parseTopic top of
    (Just (Steering _ pid)) -> writeSteerCommand queue pid $ parseSteerInput payl
    (Just (Bail _ pid)) -> writeBailCommand queue pid $ parseBailInput payl
    (Just (Join _)) -> writeJoinCommand queue $ parseJoinInput payl
    _ -> return ()

castTickThread :: TQueue Tick -> TQueue MqttMessage -> STM ()
castTickThread input output = do
    tick <- readTQueue input
    let message = (\g -> (MqttMessage "traze/1/ticker\0" (toStrict $ encode g) False)) tick
    writeTQueue output message

castGameInstancesThread :: TQueue Instance -> TQueue MqttMessage -> STM ()
castGameInstancesThread input output = do
    inst <- peekTQueue input
    let payload = (toStrict $ encode $ [(InstancesOutput (unName inst) (length $ unPlayer inst))]) 
    writeTQueue output (MqttMessage "traze/games" payload True)
    
castInstanceThread :: TQueue Instance -> TQueue MqttMessage -> STM ()
castInstanceThread input output = do
    inst <- readTQueue input 
    let topic = "traze/1/grid\0"
    let payload = (\i -> toStrict $ encode $ gridToGameState $ unGrid i) inst
    let message = MqttMessage topic payload True
    let playerPayload = (\i -> toStrict $ encode $ instanceToPlayersOutput $ i) inst
    let playerMessage = MqttMessage "traze/1/players\0" playerPayload True
    writeTQueue output message
    writeTQueue output playerMessage

castNewPlayerThread :: TQueue Player -> TQueue MqttMessage -> STM ()
castNewPlayerThread input output = do
    newP <- readTQueue input
    let topic = (\p -> "traze/1/player/"++ (unMqttClientName p) ++ "\0") newP
    let payload = (\p -> toStrict $ encode $ playerToAcceptJoinRequestOutput p) newP
    let message = MqttMessage topic payload False
    writeTQueue output message

credentialsToTuple :: Maybe Credentials -> Maybe (String, String)
credentialsToTuple Nothing = Nothing
credentialsToTuple (Just (Credentials {..})) = Just (username, password)

