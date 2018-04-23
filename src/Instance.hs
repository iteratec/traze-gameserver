module Instance (
  runInstance
) where

import InstanceTypes
import GameTypes
import GameLogic (play, getCommandPlayerId)
import SpawnQueue
import SpawnPlayer
import Colors

import Data.Maybe
import Data.List
import Data.UUID
import System.Random

-- | runs a round of interactions on a given instance.
runInstance ::RandomGen g => g -> Instance -> [Interaction] -> (Instance, [Death], [Player])
runInstance generator inst @ (Instance grid instanceName players) interactions = do
  (Instance finalGrid instanceName players', deaths, newPlayers)
  where
      commands = map fromJust $ filter isJust $ map (commandFromInteraction inst) interactions
      (grid', deaths) = play grid commands
      playersAfterRound = (onGrid players grid')
      inst' = Instance grid' instanceName playersAfterRound
      (inst'', newPlayers) = chainApply (spawnPlayerOnInstance generator) inst' interactions
      players' = playersAfterRound ++ newPlayers
      finalGrid = unGrid inst''

spawnPlayerOnInstance :: RandomGen g => g -> Instance -> Interaction -> (Instance, Maybe Player)
spawnPlayerOnInstance _ inst (GridCommand _ _) = (inst, Nothing)
spawnPlayerOnInstance generator inst @ (Instance grid instanceName players) (JoinRequest nick mqttName) =
    case isJust maybeBike of
    False -> (inst, Nothing)
    True  -> ((Instance grid' instanceName (newPlayer : players)), Just newPlayer) 
    where 
        (grid', maybeBike) = spawnPlayer grid
        pid = GameTypes.unPlayerId $ fromJust maybeBike
        initialPos = GameTypes.unCurrentLocation $ fromJust maybeBike
        newUUID :: UUID
        (newUUID, _) = random generator
        newPlayer = Player pid nick 0 0 (trazeColorStrings !! pid) newUUID mqttName initialPos

commandFromInteraction :: Instance -> Interaction -> Maybe Command
commandFromInteraction inst (GridCommand c session) = if isJust player && session == unSession (fromJust player)
    then Just c
    else Nothing
    where pid = getCommandPlayerId c
          player = getPlayerById inst pid
commandFromInteraction _ _ = Nothing

chainApply :: (a -> b -> (a, Maybe c)) -> a -> [b] -> (a, [c])
chainApply _ a [] = (a, [])
chainApply f a (b:bs) = case isJust c'' of
  False -> (a'', cs')
  True  -> (a'', (fromJust c'') : cs')
  where 
  (a', cs')  = chainApply f a bs
  (a'', c'') = f a' b

onGrid :: [Player] -> Grid -> [Player]
onGrid ps grid = filter (\p -> (InstanceTypes.unPlayerId p) `elem` (map GameTypes.unPlayerId (unBikes grid ++ map unQueueItem (unQueue grid)))) ps

getPlayerById :: Instance -> PlayerId -> Maybe Player
getPlayerById inst pid = find (\p -> pid == InstanceTypes.unPlayerId p) (unPlayer inst)

