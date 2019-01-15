module Traze.Internal.InstanceTypes where

import Traze.Internal.GameTypes
import Data.UUID

-- | a unique session id for a user
type Session        = UUID

-- | name of a game instance (instance of a grid)
type InstanceName   = String

-- | nickname of a player
type Nick           = String

-- | mqtt client name for a connection
type MqttClientName = String

-- | instance of a grid. like a game room
data Instance = Instance {
  unGrid   :: Grid,           -- ^ the grid that is played on this instance
  unName   :: InstanceName,   -- ^ name of the instance
  unPlayer :: [Player]        -- ^ list of players joind on this instance
} deriving (Show, Eq)

-- | a traze player
data Player = Player {
  playerPlayerId :: PlayerId,           -- ^ unique player identifier
  unPlayerName   :: Nick,               -- ^ player nickname
  unFrags        :: Int,                -- ^ frag (kill) count
  unDeaths       :: Int,                -- ^ death count
  unColor        :: String,             -- ^ player color in RGB 8-bit per channel
                                        --   hexadecimal format (e.g. #D327FA)
  unSession      :: Session,            -- ^ session id for that player
  unMqttClientName :: MqttClientName,   -- ^ MqttClientName of the players connection
  unInitPosition :: Coordinate          -- ^ initial position when spawning
} deriving (Show, Eq)

instance Ord Player where
  compare one two = compare (playerPlayerId one) (playerPlayerId two)

-- | player interaction with the grid instance
data Interaction 
  = GridInteraction GridCommand
  | JoinInteraction JoinRequest
  deriving (Show, Eq)

-- | command to control a bike on a grid
data GridCommand = GridCommand
                   Command      -- ^ command for the bike
                   Session    -- ^ session of the player controling the bike
  deriving (Show, Eq)

-- | request for joining a given instance
data JoinRequest = JoinRequest Nick MqttClientName
  deriving (Show, Eq)

isGridCommand :: Interaction -> Maybe GridCommand
isGridCommand (GridInteraction g) = Just g
isGridCommand _ = Nothing

isJoinRequest :: Interaction -> Maybe JoinRequest
isJoinRequest (JoinInteraction j) = Just j
isJoinRequest _ = Nothing

