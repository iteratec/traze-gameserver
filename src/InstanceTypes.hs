module InstanceTypes where

import GameTypes
import Data.UUID

type Session        = UUID
type InstanceName   = String
type Nick           = String
type MqttClientName = String

data Instance = Instance {
  unGrid   :: Grid,
  unName   :: InstanceName,
  unPlayer :: [Player]
} deriving (Show, Eq)

data Player = Player {
  unPlayerId     :: PlayerId,
  unPlayerName   :: Nick,
  unFrags        :: Int,
  unDeaths       :: Int,
  unColor        :: String,
  unSession      :: Session,
  unMqttClientName :: MqttClientName,
  unInitPosition :: Coordinate
} deriving (Show, Eq)

instance Ord Player where
  compare one two = compare (InstanceTypes.unPlayerId two) (InstanceTypes.unPlayerId one)

data Interaction 
  = GridInteraction GridCommand 
  | JoinInteraction JoinRequest
  deriving (Show, Eq)

data GridCommand = GridCommand Command UUID
  deriving (Show, Eq)

data JoinRequest = JoinRequest Nick MqttClientName
  deriving (Show, Eq)

isGridCommand :: Interaction -> Maybe GridCommand
isGridCommand (GridInteraction g) = Just g
isGridCommand _ = Nothing

isJoinRequest :: Interaction -> Maybe JoinRequest
isJoinRequest (JoinInteraction j) = Just j
isJoinRequest _ = Nothing

data Event a = Event InstanceName a
