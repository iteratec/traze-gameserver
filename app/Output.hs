{-# LANGUAGE DeriveGeneric #-}
module Output where

import GameTypes
import GameLogic
import SpawnQueue
import Instance

import Data.Char
import Data.Aeson
import Data.UUID

import GHC.Generics

data GameState = GameState {
    height :: Int,
    width :: Int,
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
      fieldLabelModifier = modifyName}
instance FromJSON OutputBike where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

type Tick = DeathTick
data DeathTick = DeathTick {
    deTiType :: String,
    deTiFragger :: PlayerId,
    deTiCasualty :: PlayerId
} deriving (Generic, Show, Eq)

instance ToJSON DeathTick where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = modifyName}
instance FromJSON DeathTick where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

data SteerInput = SteerInput {
    stInCourse :: Course,
    stInPlayerToken :: String
} deriving (Generic, Show, Eq)

instance ToJSON SteerInput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = modifyName}
instance FromJSON SteerInput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

data BailInput = BailInput {
    bailPlayerToken :: String
} deriving (Generic, Show, Eq)

instance ToJSON BailInput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = modifyName}
instance FromJSON BailInput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

data JoinInput = JoinInput {
    joInName :: String,
    joInMqttClientName :: String
} deriving (Generic, Show, Eq)

instance ToJSON JoinInput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = modifyName}
instance FromJSON JoinInput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

data AcceptJoinRequestOutput = AcceptJoinRequestOutput {
    aJROId :: Int,
    aJROName :: String,
    aJROSecretUserToken :: String,
    aJROColor :: String,
    aJROPosition :: Coordinate
} deriving (Generic, Show, Eq)

instance ToJSON AcceptJoinRequestOutput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = modifyName}
instance FromJSON AcceptJoinRequestOutput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

data InstancesOutput = InstancesOutput{
    instName :: String,
    instActivePlayers :: Int
} deriving (Generic, Show, Eq)

instance ToJSON InstancesOutput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = modifyName}
instance FromJSON InstancesOutput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

data PlayerOutput = PlayerOutput{
    plOuId :: Int,
    plOuName :: String,
    plOuColor :: String,
    plOuFrags :: Int,
    plOuOwned :: Int
} deriving (Generic, Show, Eq)

instance ToJSON PlayerOutput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = modifyName}
instance FromJSON PlayerOutput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = modifyName}

modifyName :: String -> String
modifyName input = ((toLower . head . drop 4) input) : (drop 5 input)

gridToGameState :: Grid -> GameState
gridToGameState g =
    GameState maxY maxX (getTiles g) (map getOutputBike gridBikes) (map (unCurrentLocation . unQueueItem) queue)
    where (Grid (maxX, maxY) gridBikes queue) = g

playerToAcceptJoinRequestOutput :: Player -> AcceptJoinRequestOutput
playerToAcceptJoinRequestOutput (Player pid name _ _ color session _ pos) = 
    AcceptJoinRequestOutput pid name (toString session) color pos

instanceToPlayersOutput :: Instance -> [PlayerOutput]
instanceToPlayersOutput inst = map i2pl (unPlayer inst)
    where i2pl (Player pid name frags deaths color _ _ _) = PlayerOutput pid name color frags deaths

getOutputBike :: Bike -> OutputBike
getOutputBike (Bike pid c loc tr) =
    OutputBike pid loc c tr

