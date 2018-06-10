{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Config where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Prelude hiding (lookup)
import Data.Yaml.Config (load, subconfig, lookup)

data Config = Config {
  brokerHost :: String,
  brokerPort :: Int,
  brokerUser :: String,
  brokerPassword :: String
}

getConfig :: (MonadIO m) => m (Config)
getConfig = liftIO $ do
    config <- load "./traze.yml"

    mqttConfig <- subconfig "mqtt" config
    host <- lookup "brokerHost" mqttConfig
    port <- lookup "brokerPort" mqttConfig
    user <- lookup "brokerUserName" mqttConfig
    pass <- lookup "brokerPassword" mqttConfig

    return $ Config host port user pass
