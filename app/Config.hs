{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Config where

import Prelude hiding (lookup)
import Data.Yaml.Config (load, subconfig, lookup)

data Config = Config {
  brokerHost :: String,
  brokerPort :: Int,
  brokerUser :: String,
  brokerPassword :: String,
  clientName :: String
}

getConfig :: IO (Config)
getConfig = do
    config <- load "./traze.yml"

    mqttConfig <- subconfig "mqtt" config
    host <- lookup "brokerHost" mqttConfig
    port <- lookup "brokerPort" mqttConfig
    name <- lookup "clientName" mqttConfig
    user <- lookup "brokerUserName" mqttConfig
    pass <- lookup "brokerPassword" mqttConfig

    return $ Config host port user pass name
