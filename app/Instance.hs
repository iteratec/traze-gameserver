module Instance where

import GameTypes
import GameLogic (play)
import SpawnQueue (getFirstJust)

import Data.Maybe
import Data.List
import Data.UUID

import Control.Monad (liftM)

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
runInstance :: Instance -> [Interaction] -> IO (Instance, [Death], [Player])
runInstance inst @ (Instance grid instanceName players) interactions = do
  let commands = map fromJust $ filter isJust $ map commandFromInteraction interactions
  let (grid', deaths) = play grid commands
  let playersAfterRound = (notDead players deaths)
  let inst' = Instance grid' instanceName playersAfterRound
  (inst'', newPlayers) <- chainApply spawnPlayerOnInstance inst' interactions
  let players' = playersAfterRound ++ newPlayers
  let finalGrid = unGrid inst''
  return (Instance finalGrid instanceName players', deaths, newPlayers)

spawnPlayerOnInstance :: Instance -> Interaction -> IO (Instance, Maybe Player)
spawnPlayerOnInstance inst (JoinRequest mqttClientname nick) = undefined
spawnPlayerOnInstance inst (GridCommand _) = return (inst, Nothing)

commandFromInteraction :: Interaction -> Maybe Command
commandFromInteraction (GridCommand c)   = Just c
commandFromInteraction _ = Nothing

chainApply :: (a -> b -> IO (a, Maybe c)) -> a -> [b] -> IO (a, [c])
chainApply _ a [] = return (a, [])
chainApply f a (b:bs) = do
  (a', cs')  <- chainApply f a bs
  (a'', c'') <- f a' b
  if isJust c'' then return (a'', ((fromJust c''): cs'))
  else return (a'', cs')

notDead :: [Player] -> [Death] -> [Player]
notDead ps ds = filter (\p -> (Instance.unPlayerId p) `elem` (concatMap getDeadPlayerId ds)) ps

getDeadPlayerId :: Death -> [PlayerId]
getDeadPlayerId (Suicide pid)         = [pid]
getDeadPlayerId (Frag _ pid)          = [pid]
getDeadPlayerId (Collision pid1 pid2) = pid1 : pid2 : []
