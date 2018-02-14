module Mqtt where

import GameTypes
import Instance

import qualified Data.ByteString as BS
import Control.Concurrent.STM.TQueue
import Control.Monad.STM 

data MqttMessage = MqttMessage String BS.ByteString

-- | publish 
mqttThread :: String -> TQueue BS.ByteString -> IO ()
mqttThread = undefined

castGridThread :: TQueue BS.ByteString -> TQueue Grid -> STM ()
castGridThread = undefined

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
