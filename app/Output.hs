module Output where

import GameTypes
import SpawnQueue
import Data.Maybe
import Control.Monad
import Data.List

printGrid :: Grid -> IO ()
printGrid g = mapM_ putStrLn $ gridToLineStrings g

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
getPosChar (Grid gs bikes queue) c =
    fromMaybe ' ' $ msum $ (getFrameChar gs c) : (map (getBikePosChar c) $ bikes ++ (map unQueueItem queue))

getBikePosChar :: Coordinate -> Bike -> Maybe Char
getBikePosChar c (Bike _ _ curr trail)
    | curr == c = Just 'o'
    | otherwise =
        case find (c==) (trail) of
            Just _ -> Just 'X'
            Nothing -> Nothing
