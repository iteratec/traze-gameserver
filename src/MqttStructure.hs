module MqttStructure where

import GameTypes
import InstanceTypes
import Output

import qualified Data.ByteString as BS

import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Data.Aeson
import Data.UUID
import Data.List.Split
import Data.ByteString.Lazy (fromStrict)
import Text.Read
                            -- Topic  Payload       Retain Message
data MqttMessage = MqttMessage String BS.ByteString Bool
    deriving (Show, Eq)

data MessageType
    = Steering InstanceName PlayerId
    | Bail InstanceName PlayerId
    | Join InstanceName
    deriving (Show, Eq)

parseTopic :: String -> Maybe MessageType
parseTopic top = case (splitOn "/" top) of
     ("traze" : instN : pid : "steer" : []) -> getSteering instN (readMaybe pid)
     ("traze" : instN : pid : "bail"  : []) -> getBail instN (readMaybe pid)
     ("traze" : instN : "join" : []) -> Just $ Join instN
     _ -> Nothing

     where getSteering instN (Just pid) = Just $ Steering instN pid
           getSteering _ Nothing = Nothing

           getBail instN (Just pid) = Just $ Bail instN pid
           getBail _ Nothing = Nothing

parseSteerInput :: BS.ByteString -> Maybe SteerInput
parseSteerInput bs = decode $ fromStrict bs

parseBailInput :: BS.ByteString -> Maybe BailInput
parseBailInput bs = decode $ fromStrict bs

parseJoinInput :: BS.ByteString -> Maybe JoinInput
parseJoinInput bs = decode $ fromStrict bs

writeSteerCommand :: TQueue Interaction -> PlayerId -> Maybe SteerInput -> STM ()
writeSteerCommand _ _ Nothing = return ()
writeSteerCommand queue pid (Just stin) = case uuid of
    Just session ->  writeTQueue queue $ GridInteraction $ GridCommand (MoveCommand pid (Steer $ stInCourse stin)) session
    Nothing -> return ()
    where uuid = (Data.UUID.fromString $ stInPlayerToken stin)

writeBailCommand :: TQueue Interaction -> PlayerId -> Maybe BailInput -> STM ()
writeBailCommand _ _ Nothing = return ()
writeBailCommand queue pid (Just input) = case uuid of
    Just session -> writeTQueue queue $ GridInteraction $ GridCommand (Quit pid) session
    Nothing -> return ()
    where uuid = (Data.UUID.fromString $ bailPlayerToken input)

writeJoinCommand :: TQueue Interaction -> Maybe JoinInput -> STM()
writeJoinCommand _ Nothing = return ()
writeJoinCommand queue (Just joinInput) = writeTQueue queue $ JoinInteraction $ JoinRequest (joInName joinInput) (joInMqttClientName joinInput)
    
