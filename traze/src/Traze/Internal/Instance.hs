{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Instance
Description : running an instance
Copyright   : (c) Benjamin Brunzel, 2018
License     : BSD3
Maintainer  : benjamin.brunzel@gmail.com

This module provides functions for creating a
game instance as well as spawning players and
running the actual instance.
-}
module Traze.Internal.Instance (
  stepInstance,
  runSpawning,
  applyDeath,
  incrementPlayerFrag,
  spawnPlayerOnInstance
) where

import Traze.Internal.InstanceTypes as IT
import Traze.Internal.GameTypes as GT
import Traze.Internal.GameLogic (play, isOnGrid, isInQueue)
import Traze.Internal.SpawnPlayer
import Traze.Internal.Colors

import Data.Maybe
import Data.List

import Control.Monad.State
import Control.Monad.Random

-- | perform a game step in the state monad
stepInstance :: (MonadState Instance m) => [GridCommand]   -- ^ player commands for this round
                                        -> m [Death]       -- ^ new state and list of
                                                           --   resulting deaths
stepInstance interactions = do
  inst @ (Instance grid instanceName players) <- get
  let commands = mapMaybe (commandFromInteraction inst) $ map GridInteraction interactions
      (grid', deaths) = play grid commands
      alivePlayers = playersAfterDeaths players deaths
      removeTimeouts p = (isOnGrid grid' (playerPlayerId p)) || (isInQueue grid' (playerPlayerId p))
      playersAfterRound = filter removeTimeouts alivePlayers
  put (Instance grid' instanceName playersAfterRound)
  return deaths

-- | spawn new players on the grid. (Spawning only one per round)
runSpawning :: (MonadRandom m, MonadState Instance m) => [JoinRequest] -> m [Either String Player]
runSpawning interactions = mapM joinPlayerOrError interactions

joinPlayerOrError :: (MonadRandom m, MonadState Instance m) => JoinRequest -> m (Either String Player)
joinPlayerOrError jr @ (JoinRequest _ mqttClientName) = do
    spawnedPlayer <- spawnPlayerOnInstance jr
    case spawnedPlayer of
        Nothing -> return (Left mqttClientName)
        Just player -> return (Right player)

-- | spawn a player on the instance
spawnPlayerOnInstance :: (MonadRandom m, MonadState Instance m) => JoinRequest -> m (Maybe Player)
spawnPlayerOnInstance (JoinRequest nick mqttName) = do
  Instance grid instanceName players <- get
  if nick `elem` (map unPlayerName players) then return Nothing else do
      let (grid', maybeBike) = spawnPlayer grid
      case maybeBike of
        Nothing -> return Nothing
        Just bike -> do
          newUUID <- getRandom
          let pid = bikePlayerId bike
              initialPos = GT.unCurrentLocation bike
              playerColor = (trazeColorStrings !! pid)
              newPlayer = Player pid nick 0 0 playerColor newUUID mqttName initialPos
          put (Instance grid' instanceName (newPlayer : players))
          return $ Just newPlayer

-- | check session and generate the resulting command
commandFromInteraction :: Instance -> Interaction -> Maybe Command
commandFromInteraction inst (GridInteraction (GridCommand c session)) =
  if isJust player && session == unSession (fromJust player)
    then Just c
    else Nothing
    where pid = getCommandPlayerId c
          player = getPlayerById inst pid
commandFromInteraction _ _ = Nothing

-- | apply deaths to the player list
playersAfterDeaths :: [Player] -> [Death] -> [Player]
playersAfterDeaths ps ds = foldr applyDeath ps ds

-- | apply death to the player list. removing dead players and incrementing frag counts
applyDeath :: Death -> [Player] -> [Player]
applyDeath (Suicide pid) ps = removePlayer pid ps
applyDeath (Collision one two) ps = filter notColided ps
  where notColided = (\p -> not ((playerPlayerId p) `elem` one : two : []))
applyDeath (Frag killer casulty) ps = (sort . (removePlayer casulty) . (incrementFragCount killer)) ps

-- | remove player with playerId
removePlayer :: PlayerId -> [Player] -> [Player]
removePlayer pid ps = filter (\p -> not ((playerPlayerId p) == pid)) ps

-- | increment frag count for the player with a given id
incrementFragCount :: PlayerId -> [Player] -> [Player]
incrementFragCount pid ps =
  case findPlayerById pid ps of
    Nothing -> ps
    Just player -> (incrementPlayerFrag player) : removePlayer pid ps

-- | increment the frag count of a given player
incrementPlayerFrag :: Player -> Player
incrementPlayerFrag Player {..} = Player playerPlayerId unPlayerName (unFrags + 1)
  unDeaths unColor unSession unMqttClientName unInitPosition

findPlayerById :: PlayerId -> [Player] -> Maybe Player
findPlayerById pid = find (\p -> ((playerPlayerId p) == pid))

getPlayerById :: Instance -> PlayerId -> Maybe Player
getPlayerById inst pid = findPlayerById pid (unPlayer inst)
