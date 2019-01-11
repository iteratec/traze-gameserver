{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Traze.Internal.Instance (
  stepInstance,
  runSpawning,
  applyDeath,
  incrementPlayerFrag
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

stepInstance :: (MonadState Instance m) => [GridCommand] -> m [Death]
stepInstance interactions = do
  inst @ (Instance grid instanceName players) <- get
  let commands = mapMaybe (commandFromInteraction inst) $ map GridInteraction interactions
      (grid', deaths) = play grid commands
      alivePlayers = playersAfterDeaths players deaths
      removeTimeouts p = (isOnGrid grid' (playerPlayerId p)) || (isInQueue grid' (playerPlayerId p))
      playersAfterRound = filter removeTimeouts alivePlayers
  put (Instance grid' instanceName playersAfterRound)
  return deaths

runSpawning :: (MonadRandom m, MonadState Instance m) => [JoinRequest] -> m [Player]
runSpawning interactions = do
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
      let pid = bikePlayerId bike
          initialPos = GT.unCurrentLocation bike
          playerColor = (trazeColorStrings !! pid)
          newPlayer = Player pid nick 0 0 playerColor newUUID mqttName initialPos
      put (Instance grid' instanceName (newPlayer : players))
      return $ Just newPlayer

commandFromInteraction :: Instance -> Interaction -> Maybe Command
commandFromInteraction inst (GridInteraction (GridCommand c session)) = 
  if isJust player && session == unSession (fromJust player)
    then Just c
    else Nothing
    where pid = getCommandPlayerId c
          player = getPlayerById inst pid
commandFromInteraction _ _ = Nothing

playersAfterDeaths :: [Player] -> [Death] -> [Player]
playersAfterDeaths ps ds = foldr applyDeath ps ds 

applyDeath :: Death -> [Player] -> [Player]
applyDeath (Suicide pid) ps = removePlayer pid ps
applyDeath (Collision one two) ps = filter notColided ps
  where notColided = (\p -> not ((playerPlayerId p) `elem` one : two : []))
applyDeath (Frag killer casulty) ps = (sort . (removePlayer casulty) . (incrementFragCount killer)) ps

removePlayer :: PlayerId -> [Player] -> [Player]
removePlayer pid ps = filter (\p -> not ((playerPlayerId p) == pid)) ps

incrementFragCount :: PlayerId -> [Player] -> [Player]
incrementFragCount pid ps = 
  case findPlayerById pid ps of 
    Nothing -> ps
    Just player -> (incrementPlayerFrag player) : removePlayer pid ps

incrementPlayerFrag :: Player -> Player
incrementPlayerFrag Player {..} = Player playerPlayerId unPlayerName (unFrags + 1) 
  unDeaths unColor unSession unMqttClientName unInitPosition

findPlayerById :: PlayerId -> [Player] -> Maybe Player
findPlayerById pid = find (\p -> ((playerPlayerId p) == pid))

getPlayerById :: Instance -> PlayerId -> Maybe Player
getPlayerById inst pid = findPlayerById pid (unPlayer inst)

