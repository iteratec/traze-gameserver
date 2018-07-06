{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad.State
import Control.Monad.Random

-- | runs a round of interactions on a given instance.
runInstance :: (MonadRandom m, MonadState Instance m) => [Interaction] -> m ([Death], [Player])
runInstance interactions = do
  inst @ (Instance grid instanceName players) <- get
  let commands = mapMaybe (commandFromInteraction inst) interactions
      (grid', deaths) = play grid commands
      playersAfterRound = (onGrid players grid')
  put (Instance grid' instanceName players)
  newPlayers <- catMaybes <$> mapM spawnPlayerOnInstance interactions
  let players' = playersAfterRound ++ newPlayers
  finalGrid <- gets unGrid

  put (Instance finalGrid instanceName players')
  return (deaths, newPlayers)

spawnPlayerOnInstance :: (MonadRandom m, MonadState Instance m) => Interaction -> m (Maybe Player)
spawnPlayerOnInstance (GridCommand _ _) = return Nothing
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
commandFromInteraction inst (GridCommand c session) = if isJust player && session == unSession (fromJust player)
    then Just c
    else Nothing
    where pid = getCommandPlayerId c
          player = getPlayerById inst pid
commandFromInteraction _ _ = Nothing

onGrid :: [Player] -> Grid -> [Player]
onGrid ps grid = filter (\p -> (InstanceTypes.unPlayerId p) `elem` (map GameTypes.unPlayerId (unBikes grid ++ map unQueueItem (unQueue grid)))) ps

getPlayerById :: Instance -> PlayerId -> Maybe Player
getPlayerById inst pid = find (\p -> pid == InstanceTypes.unPlayerId p) (unPlayer inst)

