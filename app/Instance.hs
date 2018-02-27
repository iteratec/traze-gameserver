module Instance where

import GameTypes
import GameLogic (play)
import SpawnQueue (getFirstJust)

import Data.Maybe
import Data.List
import Data.UUID

type Session        = UUID
type InstanceName   = String
type Nick           = String
type MqttClientName = String

data Instance = Instance {
  unGrid   :: Grid,
  unName   :: String,
  unPlayer :: [Player]
}

data Player = Player {
  unPlayerId   :: PlayerId,
  unPlayerName :: String,
  unFrags      :: Int,
  unDeaths     :: Int,
  unColor      :: String,
  unSession    :: Session
}

data Interaction 
  = GridCommand Command
  | JoinRequest MqttClientName Nick

-- | runs a round of interactions on a given instance.
runInstance :: Instance -> [Interaction] -> (Instance, [Death], [Player])
runInstance inst @ (Instance grid instanceName players) interactions = 
  (Instance finalGrid instanceName players', deaths, newPlayers)
  where (grid', deaths)      = play grid commands
        players'             = playersAfterRound ++ newPlayers
        playersAfterRound    = (notDead players deaths)
        inst'                = Instance grid' instanceName playersAfterRound
        (inst'', newPlayers) = chainApply spawnPlayer inst' interactions
        finalGrid            = unGrid inst''
        commands             = map fromJust $ filter isJust $ map commandFromInteraction interactions

spawnPlayer :: Instance -> Interaction -> (Instance, Maybe Player)
spawnPlayer inst (JoinRequest mqttClientname nick) = undefined
spawnPlayer inst (GridCommand _) = (inst, Nothing)

commandFromInteraction :: Interaction -> Maybe Command
commandFromInteraction (GridCommand c)   = Just c
commandFromInteraction _ = Nothing

chainApply :: (a -> b -> (a, Maybe c)) -> a -> [b] -> (a, [c])
chainApply _ a [] = (a, [])
chainApply f a (b:bs)
  | isJust c''     = (a'', ((fromJust c''): cs'))
  | otherwise      = (a'', cs')
  where (a'', c'') = f a' b
        (a', cs')  = chainApply f a bs

notDead :: [Player] -> [Death] -> [Player]
notDead ps ds = filter (\p -> (Instance.unPlayerId p) `elem` (concatMap getDeadPlayerId ds)) ps

getDeadPlayerId :: Death -> [PlayerId]
getDeadPlayerId (Suicide pid)         = [pid]
getDeadPlayerId (Frag _ pid)          = [pid]
getDeadPlayerId (Collision pid1 pid2) = pid1 : pid2 : []
