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

import Data.ByteString.Char8 (pack)

import qualified Network.Mosquitto as M

data MqttMessage = MqttMessage String BS.ByteString

-- | publish 
mqttThread :: TQueue (String, BS.ByteString) -> Config -> IO ()
mqttThread queue config = M.withMosquittoLibrary $ do
    m <- M.newMosquitto True (clientName config) (Just ())
    M.setTlsInsecure m True
    _ <- M.setReconnectDelay m True 2 30
    M.onMessage m print
    M.onLog m $ const putStrLn
    _ <- M.onConnect m $ \c -> do
        print c

    M.onDisconnect m print
    M.onSubscribe m $ curry print
    _ <- M.connect m (brokerHost config) (brokerPort config) 1200

    _ <- forkIO $ forever $ do
        (topic, message) <- atomically $ readTQueue queue
        M.publish m False 0 topic message
    return ()

castGridThread :: TQueue Grid -> TQueue (String, BS.ByteString) -> STM ()
castGridThread input output = do
    grid <- readTQueue input 
    let message = (\g -> ("/traze/1/grid", pack $ gridToString g)) grid
    writeTQueue output message

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
