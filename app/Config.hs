{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Config where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Prelude hiding (lookup)
import Data.Yaml.Config (load, subconfig, lookup)

import Debug.Trace

data Config = Config {
  brokerHost :: String,
  brokerPort :: Int,
  brokerUser :: String,
  brokerPassword :: String,
  instances :: [InstanceConfig]
} deriving (Eq, Show)

data InstanceConfig = InstanceConfig {
  instanceName :: String
} deriving (Eq, Show)

getConfig :: (MonadIO m) => m (Config)
getConfig = liftIO $ do
    config <- load "./traze.yml"

    mqttConfig <- subconfig "mqtt" config
    host <- lookup "brokerHost" mqttConfig
    port <- lookup "brokerPort" mqttConfig
    user <- lookup "brokerUserName" mqttConfig
    pass <- lookup "brokerPassword" mqttConfig

    yamlInstances <- lookup "instances" config
    let instances = fmap InstanceConfig yamlInstances

    let result = Config host port user pass instances
    return $ traceShowId result
