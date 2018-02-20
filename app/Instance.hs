module Instance where

import GameTypes
import Data.UUID

type Session = UUID
type InstanceName = String

data Instance = Instance {
  unGrid :: Grid,
  unName :: String,
  unPlayer :: [Player]
}

data Player = Player {
  unPlayerId :: PlayerId,
  unPlayerName :: String,
  unFrags :: Int,
  unDeaths :: Int,
  unColor :: String,
  unSession :: Session
}

