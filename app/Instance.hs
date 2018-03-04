module Instance where

import GameTypes
import GameLogic (play, getCommandPlayerId)
import SpawnQueue
import SpawnPlayer

import Data.Maybe
import Data.List
import Data.UUID
import System.Random

import Control.Monad (liftM)

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
  unInitPosition :: Coordinate
} deriving (Show, Eq)

data Interaction 
  = GridCommand Command UUID
  | JoinRequest MqttClientName Nick

-- | runs a round of interactions on a given instance.
runInstance :: Instance -> [Interaction] -> IO (Instance, [Death], [Player])
runInstance inst @ (Instance grid instanceName players) interactions = do
  let commands = map fromJust $ filter isJust $ map (commandFromInteraction inst) interactions
  let (grid', deaths) = play grid commands
  let playersAfterRound = (onGrid players grid')
  let inst' = Instance grid' instanceName playersAfterRound
  (inst'', newPlayers) <- chainApply spawnPlayerOnInstance inst' interactions
  let players' = playersAfterRound ++ newPlayers
  let finalGrid = unGrid inst''
  return (Instance finalGrid instanceName players', deaths, newPlayers)

spawnPlayerOnInstance :: Instance -> Interaction -> IO (Instance, Maybe Player)
spawnPlayerOnInstance inst (GridCommand _ _) = return (inst, Nothing)
spawnPlayerOnInstance inst @ (Instance grid instanceName players) (JoinRequest mqttClientname nick) = do
  let (grid', maybeBike) = spawnPlayer grid
  if isJust maybeBike then do
    let pid = GameTypes.unPlayerId $ fromJust maybeBike
    let initialPos =  GameTypes.unCurrentLocation $ fromJust maybeBike
    newUUID <- randomIO
    let newPlayer = Player pid nick 0 0 "#b1147a" newUUID initialPos
    return $ ((Instance grid' instanceName (newPlayer : players)), Just newPlayer)
  else
    return (inst, Nothing)

commandFromInteraction :: Instance -> Interaction -> Maybe Command
commandFromInteraction inst (GridCommand c session) = if isJust player && session == unSession (fromJust player)
    then Just c
    else Nothing
    where pid = getCommandPlayerId c
          player = getPlayerById inst pid
commandFromInteraction _ _ = Nothing

chainApply :: (a -> b -> IO (a, Maybe c)) -> a -> [b] -> IO (a, [c])
chainApply _ a [] = return (a, [])
chainApply f a (b:bs) = do
  (a', cs')  <- chainApply f a bs
  (a'', c'') <- f a' b
  if isJust c'' then return (a'', ((fromJust c''): cs'))
  else return (a'', cs')

onGrid :: [Player] -> Grid -> [Player]
onGrid ps grid = filter (\p -> (Instance.unPlayerId p) `elem` (map GameTypes.unPlayerId (unBikes grid ++ map unQueueItem (unQueue grid)))) ps

getPlayerById :: Instance -> PlayerId -> Maybe Player
getPlayerById inst pid = find (\p -> pid == Instance.unPlayerId p) (unPlayer inst)

getDeadPlayerId :: Death -> [PlayerId]
getDeadPlayerId (Suicide pid)         = [pid]
getDeadPlayerId (Frag _ pid)          = [pid]
getDeadPlayerId (Collision pid1 pid2) = pid1 : pid2 : []
