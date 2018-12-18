{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Instance (
  stepInstance,
  runSpawning,
  spawnPlayerOnInstance
) where

import InstanceTypes
import GameTypes
import GameLogic (play, getCommandPlayerId)
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
      playersAfterRound = playersAfterDeaths players deaths
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
      let pid = GameTypes.unPlayerId bike
          initialPos = GameTypes.unCurrentLocation bike
          newPlayer = Player pid nick 0 0 (trazeColorStrings !! pid) newUUID mqttName initialPos
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
applyDeath (Collision one two) ps = filter (\p -> not ((InstanceTypes.unPlayerId p) `elem` one : two : [])) ps
applyDeath (Frag killer casulty) ps =  (sort . (removePlayer casulty) . (incrementFragCount killer)) ps

removePlayer :: PlayerId -> [Player] -> [Player]
removePlayer pid ps = filter (\p -> not ((InstanceTypes.unPlayerId p) == pid)) ps

incrementFragCount :: PlayerId -> [Player] -> [Player]
incrementFragCount pid ps = (incrementFrag (fromJust $ findPlayerById pid ps)) : removePlayer pid ps

incrementFrag :: Player -> Player
incrementFrag Player {..} = Player unPlayerId unPlayerName (unFrags + 1) unDeaths unColor unSession unMqttClientName unInitPosition

findPlayerById :: PlayerId -> [Player] -> Maybe Player
findPlayerById pid = find (\p -> ((InstanceTypes.unPlayerId p) == pid))

getPlayerById :: Instance -> PlayerId -> Maybe Player
getPlayerById inst pid = findPlayerById pid (unPlayer inst)

