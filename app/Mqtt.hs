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

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict, fromStrict)

import qualified Network.Mosquitto as M
import Network.Mosquitto.Internal.Types

data MqttMessage = MqttMessage String BS.ByteString

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

    M.onDisconnect m print
    M.onSubscribe m $ curry print
    M.connect m (brokerHost config) (brokerPort config) 1200

    _ <- forkIO $ forever $ do
        (topic, message) <- atomically $ readTQueue gridQueue
        M.publish m False 0 topic message
    M.loopForever m
    M.destroyMosquitto m
    return ()

castGridThread :: TQueue Grid -> TQueue (String, BS.ByteString) -> STM ()
castGridThread input output = do
    grid <- readTQueue input 
    let message = (\g -> ("traze/1/grid", toStrict $ encode $ gridToGameState g)) grid
    writeTQueue output message

handleMessage :: TQueue Command -> Message -> STM ()
handleMessage queue (Message _ top payl _ _) = case (parseTopic top, parseSteerInput payl) of
    (Just id, Just stin) -> writeTQueue queue (MoveCommand id (Steer $ course stin))
    (_, _) -> return ()
    
parseTopic :: String -> Maybe PlayerId
parseTopic top = case (splitOn "/" top) of
     ("traze" : _ : id : "steer" : []) -> Just (read id)
     _ -> Nothing

parseSteerInput :: BS.ByteString -> Maybe SteerInput
parseSteerInput bs = decode $ fromStrict bs

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
