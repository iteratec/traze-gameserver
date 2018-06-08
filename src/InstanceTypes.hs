module InstanceTypes where

import GameTypes
import Data.UUID

type Session        = UUID
type InstanceName   = String
type Nick           = String
type MqttClientName = String

data Instance = Instance {
  unGrid   :: Grid,
  unName   :: String,
  unPlayer :: [Player]
} deriving (Show, Eq)

data Player = Player {
  unPlayerId     :: PlayerId,
  unPlayerName   :: String,
  unFrags        :: Int,
  unDeaths       :: Int,
  unColor        :: String,
  unSession      :: Session,
  unMqttClientName :: String,
  unInitPosition :: Coordinate
} deriving (Show, Eq)

data Interaction 
  = GridCommand Command UUID
  | JoinRequest Nick MqttClientName
  deriving (Show, Eq)

