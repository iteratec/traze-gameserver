{-# LANGUAGE DeriveGeneric #-}
module Traze.Internal.Output where

import Traze.Internal.GameTypes
import Traze.Internal.InstanceTypes
import Traze.Internal.GameLogic
import Traze.Internal.SpawnQueue
import Traze.Internal.Utils

import Data.Aeson
import Data.UUID

import GHC.Generics

-- https://traze.iteratec.de/#select-an-game-instance
data InstancesOutput = InstancesOutput{
    instName :: String,
    instActivePlayers :: Int
} deriving (Generic, Show, Eq)

instance ToJSON InstancesOutput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON InstancesOutput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

-- https://traze.iteratec.de/#grid-information
data GameState = GameState {
    height :: Int,
    width :: Int,
    currentTick :: Int,
    gameStartTick :: Int,
    tickMs :: Int,
    tiles :: [[Int]],
    bikes :: [OutputBike],
    spawns :: [Coordinate]
} deriving (Generic, Show, Eq)

instance ToJSON GameState where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON GameState

data OutputBike = OutputBike {
    ouBiPlayerId :: Int,
    ouBiCurrentLocation :: Coordinate,
    ouBiDirection :: Course,
    ouBiTrail :: [Coordinate]
} deriving (Generic, Show, Eq)

instance ToJSON OutputBike where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON OutputBike where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

-- https://traze.iteratec.de/#player-information
data PlayerOutput = PlayerOutput{
    plOuId :: Int,
    plOuName :: String,
    plOuColor :: String,
    plOuFrags :: Int,
    plOuOwned :: Int
} deriving (Generic, Show, Eq)

instance ToJSON PlayerOutput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON PlayerOutput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

-- https://traze.iteratec.de/#ticker
type Tick = DeathTick
data DeathTick = DeathTick {
    deTiType :: String,
    deTiFragger :: PlayerId,
    deTiCasualty :: PlayerId
} deriving (Generic, Show, Eq)

instance ToJSON DeathTick where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON DeathTick where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

-- https://traze.iteratec.de/#joining-the-game
data JoinInput = JoinInput {
    joInName :: String,
    joInMqttClientName :: String
} deriving (Generic, Show, Eq)

instance ToJSON JoinInput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON JoinInput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

-- https://traze.iteratec.de/#joining-the-game
data JoinRequestOutput =
    AcceptJoinRequestOutput {
        aJROId :: Int,
        aJROName :: String,
        aJROSecretUserToken :: String,
        aJROColor :: String,
        aJROPosition :: Coordinate
    }
  | DenyJoinRequestOutput {
        djroName :: String,
        djroJoinStatus :: String
    }
  deriving (Generic, Show, Eq)

instance ToJSON JoinRequestOutput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON JoinRequestOutput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

-- https://traze.iteratec.de/#steering-your-light-cycle
data SteerInput = SteerInput {
    stInCourse :: Course,
    stInPlayerToken :: String
} deriving (Generic, Show, Eq)

instance ToJSON SteerInput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON SteerInput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

-- https://traze.iteratec.de/#leaving-the-game
data BailInput = BailInput {
    bailPlayerToken :: String
} deriving (Generic, Show, Eq)

instance ToJSON BailInput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}
instance FromJSON BailInput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = cut4LowerCase}

gridToGameState :: Grid -> GameState
gridToGameState g =
    GameState maxY maxX ticks 0 250
      (getTiles g)
      (map getOutputBike gridBikes)
      (map (unCurrentLocation . retrieveQueueItem) queue)
    where (Grid (maxX, maxY) gridBikes queue ticks) = g

playerToJoinRequestOutput :: Either String Player -> JoinRequestOutput
playerToJoinRequestOutput (Left nickname) = DenyJoinRequestOutput nickname "Nickname already taken"
playerToJoinRequestOutput (Right (Player pid name _ _ color session _ pos)) =
    AcceptJoinRequestOutput pid name (toString session) color pos

instanceToPlayersOutput :: Instance -> [PlayerOutput]
instanceToPlayersOutput inst = map i2pl (unPlayer inst)
    where i2pl (Player pid name frags deaths color _ _ _) = PlayerOutput pid name color frags deaths

getOutputBike :: Bike -> OutputBike
getOutputBike (Bike pid c loc tr) =
    OutputBike pid loc c tr
