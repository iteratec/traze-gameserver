{-# LANGUAGE DeriveGeneric #-}
module Output where

import GameTypes
import SpawnQueue

import Data.Maybe
import Control.Monad
import Data.List
import Data.Aeson

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
    playerId :: Int,
    currentLocation :: Coordinate,
    direction :: Course,
    trail :: [Coordinate]
} deriving (Generic, Show, Eq)

instance ToJSON OutputBike where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON OutputBike

data SteerInput = SteerInput {
    stInCourse :: Course,
    stInPlayerToken :: String
} deriving (Generic, Show, Eq)

instance ToJSON SteerInput where
    toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = drop 4 }
instance FromJSON SteerInput where
    parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = drop 4}

data BailInput = BailInput {
    playerToken :: String
} deriving (Generic, Show, Eq)

instance ToJSON BailInput where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON BailInput

gridToGameState :: Grid -> GameState
gridToGameState g =
    GameState maxY maxX (getTiles g) (map getOutputBike gridBikes)
    where (Grid (maxX, maxY) gridBikes _) = g

getTiles :: Grid -> [[Int]]
getTiles g = (map . map) (getPosPlayerId gridBikes) (getGridCoords gs)
    where (Grid gs gridBikes _) = g

getPosPlayerId :: [Bike] -> Coordinate -> PlayerId
getPosPlayerId bs c = fromMaybe 0 $ getFirstJust $ map (getPid c) bs

getPid :: Coordinate -> Bike -> Maybe PlayerId
getPid c b
    | c `elem` (unTrail b) = Just (unPlayerId b)
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
    map (getLineCoords (maxX, maxY)) $ map ((-) maxY) [(0)..(maxY+1)]

getLineCoords :: GridSize -> Int -> [Coordinate]
getLineCoords (maxX,_) y = [(x,y) | x <- [(-1)..(maxX)]]

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
