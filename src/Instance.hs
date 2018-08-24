{-# LANGUAGE FlexibleContexts #-}

module Instance (
  stepInstance,
  runSpawning,
  spawnPlayerOnInstance
) where

import InstanceTypes
import GameTypes
import GameLogic (play, getCommandPlayerId)
import SpawnQueue
import SpawnPlayer
import Colors

import Data.Maybe
import Data.List

import Control.Monad.State
import Control.Monad.Random

stepInstance :: (MonadState Instance m) => [GridCommand] -> m [Death]
stepInstance interactions = do
  inst @ (Instance grid instanceName players) <- get
  let commands = mapMaybe (commandFromInteraction inst) $ map GridInteraction interactions
      (grid', deaths) = play grid commands
      playersAfterRound = onGrid players grid'
  put (Instance grid' instanceName playersAfterRound)
  return deaths

runSpawning :: (MonadRandom m, MonadState Instance m) => [JoinRequest] -> m [Player]
runSpawning interactions = do
  inst @ (Instance grid instanceName players) <- get
  newPlayers <- catMaybes <$> mapM spawnPlayerOnInstance interactions
  return newPlayers

spawnPlayerOnInstance :: (MonadRandom m, MonadState Instance m) => JoinRequest -> m (Maybe Player)
spawnPlayerOnInstance (JoinRequest nick mqttName) = do
  Instance grid instanceName players <- get
  let (grid', maybeBike) = spawnPlayer grid
  case maybeBike of
    Nothing -> return Nothing
    Just bike -> do
      newUUID <- getRandom
      let pid = GameTypes.unPlayerId bike
          initialPos = GameTypes.unCurrentLocation bike
          newPlayer = Player pid nick 0 0 (trazeColorStrings !! pid) newUUID mqttName initialPos
      put (Instance grid' instanceName (newPlayer : players))
      return $ Just newPlayer

commandFromInteraction :: Instance -> Interaction -> Maybe Command
commandFromInteraction inst (GridInteraction (GridCommand c session)) = if isJust player && session == unSession (fromJust player)
    then Just c
    else Nothing
    where pid = getCommandPlayerId c
          player = getPlayerById inst pid
commandFromInteraction _ _ = Nothing

onGrid :: [Player] -> Grid -> [Player]
onGrid ps grid = filter (\p -> (InstanceTypes.unPlayerId p) `elem` (map GameTypes.unPlayerId (unBikes grid ++ map unQueueItem (unQueue grid)))) ps

getPlayerById :: Instance -> PlayerId -> Maybe Player
getPlayerById inst pid = find (\p -> pid == InstanceTypes.unPlayerId p) (unPlayer inst)

