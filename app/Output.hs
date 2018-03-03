{-# LANGUAGE DeriveGeneric #-}
module Output where

import GameTypes
import SpawnQueue
import Instance

import Data.Maybe
import Control.Monad
import Data.List
import Data.Char
import Data.Aeson
import Data.UUID

import GHC.Generics

data GameState = GameState {
    height :: Int,
    width :: Int,
    tiles :: [[Int]],
    bikes :: [OutputBike]
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
    deTiCasualty :: PlayerId,
    deTiFragger :: PlayerId
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
    joInName :: String
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
    plOuId :: String,
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
    GameState maxY maxX (getTiles g) (map getOutputBike gridBikes)
    where (Grid (maxX, maxY) gridBikes _) = g

playerToAcceptJoinRequestOutput :: Player -> AcceptJoinRequestOutput
playerToAcceptJoinRequestOutput (Player pid name _ _ color session pos) = 
    AcceptJoinRequestOutput pid name (toString session) color pos

instanceToPlayersOutput :: Instance -> [PlayerOutput]
instanceToPlayersOutput inst = map i2pl (unPlayer inst)
    where i2pl (Player pid name frags deaths color _ _) = PlayerOutput (show pid) name color frags deaths

getTiles :: Grid -> [[Int]]
getTiles g = (map . map) (getPosPlayerId gridBikes) (getGridCoords gs)
    where (Grid gs gridBikes _) = g

getPosPlayerId :: [Bike] -> Coordinate -> PlayerId
getPosPlayerId bs c = fromMaybe 0 $ getFirstJust $ map (getPid c) bs

getPid :: Coordinate -> Bike -> Maybe PlayerId
getPid c b
    | c `elem` (unTrail b) = Just (GameTypes.unPlayerId b)
    | otherwise = Nothing

getOutputBike :: Bike -> OutputBike
getOutputBike (Bike pid c loc tr) =
    OutputBike pid loc c tr

printGrid :: Grid -> IO ()
printGrid = mapM_ putStrLn . gridToLineStrings

gridToString :: Grid -> String
gridToString = intercalate "\n" . gridToLineStrings

gridToLineStrings :: Grid -> [String]
gridToLineStrings g = (map . map) (getPosChar g) $ getGridCoords $ unGridSize g

getGridCoords :: GridSize -> [[Coordinate]]
getGridCoords (maxX, maxY) =
    map (getLineCoords (maxX, maxY)) $ [0..maxX]

getLineCoords :: GridSize -> Int -> [Coordinate]
getLineCoords (maxY,_) x = [(x,y) | y <- [0..maxY]]

getFrameChar :: GridSize -> Coordinate -> Maybe Char
getFrameChar (maxX, maxY) (x, y)
    | isOut x maxX && isOut y maxY = Just '+'
    | isOut x maxX = Just '|'
    | isOut y maxY = Just '-'
    | otherwise = Nothing
    where isOut a bound = a >= bound || a < 0

getPosChar :: Grid -> Coordinate -> Char
getPosChar (Grid gs bs queue) c =
    fromMaybe ' ' $ msum $ (getFrameChar gs c) : (map (getBikePosChar c) $ bs ++ (map unQueueItem queue))

getBikePosChar :: Coordinate -> Bike -> Maybe Char
getBikePosChar c (Bike _ _ curr tr)
    | curr == c = Just 'o'
    | otherwise =
        case find (c==) (tr) of
            Just _ -> Just 'X'
            Nothing -> Nothing
