{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Config ( Config
              , Credentials (Credentials)
              , getConfig
              , brokerHost
              , brokerPort
              , brokerCredentials
              , instances
              , username
              , password
              ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Prelude hiding (lookup)
import Data.Monoid
import Data.Semigroup (Semigroup)
import System.Exit

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Debug.Trace

data Config = Config {
  brokerHost :: String,
  brokerPort :: Int,
  brokerCredentials :: Maybe Credentials,
  instances :: [InstanceConfig]
} deriving (Eq, Show)

data Credentials = Credentials {
  username :: String,
  password :: String
} deriving (Eq, Show)

data PartialConfig = PartialConfig {
  pcBrokerHost :: Last String,
  pcBrokerPort :: Last Int,
  pcBrokerUser :: Last String,
  pcBrokerPassword :: Last String,
  pcInstances :: [InstanceConfig]
} deriving (Eq, Show)

instance Semigroup PartialConfig where
 (<>) x y = PartialConfig
    { pcBrokerHost = pcBrokerHost x <> pcBrokerHost y
    , pcBrokerPort = pcBrokerPort x <> pcBrokerPort y
    , pcBrokerUser = pcBrokerUser x <> pcBrokerUser y
    , pcBrokerPassword = pcBrokerPassword x <> pcBrokerPassword y
    , pcInstances = pcInstances x <> pcInstances y
    }

instance Monoid PartialConfig where
  mempty = PartialConfig mempty mempty mempty mempty mempty
 
data InstanceConfig = InstanceConfig {
  instanceName :: String
} deriving (Eq, Show)

getConfig :: (MonadIO m) => m (Config)
getConfig = do
  environmentVariableConfig <- getEnvironmentVariableConfig
  let combinedConfig =  defaultPartialConfig
                     <> traceShowId environmentVariableConfig
  either (liftIO . die) return $ makeConfig $ traceShowId combinedConfig


lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

listToEither :: String -> [a] -> Either String [a]
listToEither errMsg [] = Left errMsg
listToEither _ a = Right a

makeCredentials :: Maybe String -> Maybe String -> Maybe Credentials
makeCredentials (Just username) (Just password ) = Just $ Credentials {..}
makeCredentials _ _ = Nothing

makeConfig :: PartialConfig -> Either String Config
makeConfig PartialConfig {..} = do
  brokerHost <- lastToEither "broker host not configured" pcBrokerHost
  brokerPort <- lastToEither "broker port not configured" pcBrokerPort
  instances <- listToEither "no instances configured" pcInstances
  let brokerCredentials = makeCredentials (getLast pcBrokerUser) (getLast pcBrokerPassword)
  return $ traceShowId Config {..}

getEnvironmentVariableConfig :: (MonadIO m) => m (PartialConfig)
getEnvironmentVariableConfig = liftIO $ do
  pcBrokerHost <- lookupEnv "TRAZE_BROKER_HOST"
  pcBrokerPort <- lookupEnv "TRAZE_BROKER_PORT"
  pcBrokerUser <- lookupEnv "TRAZE_BROKER_USER"
  pcBrokerPassword <- lookupEnv "TRAZE_BROKER_PASSWORD"
  instanceString <- lookupEnv "TRAZE_INSTANCES"
  let pcInstances = fmap InstanceConfig $ words $ fromMaybe [] instanceString
  return $ PartialConfig (Last pcBrokerHost) (Last $ readMaybe $ fromMaybe [] pcBrokerPort) (Last pcBrokerUser) (Last pcBrokerPassword) pcInstances

defaultPartialConfig :: PartialConfig
defaultPartialConfig = mempty
  { pcBrokerHost = pure "localhost"
  , pcBrokerPort = pure 1883
  , pcInstances = [ InstanceConfig "1" ]
  }

