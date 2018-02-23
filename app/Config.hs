{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Config where

import Prelude hiding (lookup)
import Data.Yaml.Config (load, subconfig, lookup)

data Config = Config {
  brokerHost :: String,
  brokerPort :: Int,
  clientName :: String
}

getConfig :: IO (Config)
getConfig = do
    config <- load "./traze.yml"

    mqttConfig <- subconfig "mqtt" config
    host <- lookup "brokerHost" mqttConfig
    port <- lookup "brokerPort" mqttConfig
    name <- lookup "clientName" mqttConfig

    return $ Config host port name
