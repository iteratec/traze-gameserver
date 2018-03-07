module SpawnPlayer where

import GameTypes
import Tuple
import SpawnQueue

import Data.List (find)
import Data.Maybe (fromJust)

spawnPlayer :: Grid -> (Grid, Maybe Bike)
spawnPlayer g = case spawnCoord of
    Nothing   -> (g, Nothing)
    Just _ -> (Grid (unGridSize g) (unBikes g) ((enQueue b) : (unQueue g)), Just b)
    where b = Bike (newPlayerId (map (unPlayerId) $ (unBikes g) ++ (map unQueueItem $ unQueue g))) N (fromJust spawnCoord) []
          spawnCoord = getSpawnCoord g

newPlayerId :: [PlayerId] -> PlayerId
newPlayerId ps = x
    where Just x = find (`notElem` ps) [1..]

getSpawnCoord :: Grid -> Maybe Coordinate
getSpawnCoord g = case free of
    []        -> Nothing
    cs -> Just $ snd $ maximum $ map (scoreAndCord g) cs
    where free = allFreeCoords g

scoreAndCord :: Grid -> Coordinate -> (Int, Coordinate)
scoreAndCord g c = (spawnScore g c, c)

spawnScore :: Grid -> Coordinate -> Int
spawnScore g c = (awayFromAnyBike g c) + 2 * (awayFromWalls g c)

allFreeCoords :: Grid -> [Coordinate]
allFreeCoords (Grid gs bs q) = filter (not . (flip elem $ allBikeTrailCords bs ++ queuedCoords)) (allCoords gs)
    where queuedCoords = (map (unCurrentLocation . retrieveQueueItem) q)

allBikeTrailCords :: [Bike] -> [Coordinate]
allBikeTrailCords bs = concatMap (\x -> ((unCurrentLocation x) : (unTrail x))) bs

allCoords :: GridSize -> [Coordinate]
allCoords (maxX, maxY) = [(x, y) | x <- [0..(maxX - 1)], y <- [0..(maxY - 1)]]

awayFromAnyBike :: Grid -> Coordinate -> Int
awayFromAnyBike (Grid _ [] _) _ = 0
awayFromAnyBike (Grid _ bs _) c = minimum $ map (flip awayFromBike c) bs

awayFromBike :: Bike -> Coordinate -> Int
awayFromBike b c = round squareRoot
    where
        squareRoot :: Double
        squareRoot  = sqrt $
            tupleFold (+) $
            tupleDo fromIntegral $
            tupleDo ((flip (^)) (2 :: Int)) $
            tupleDo abs $
            tupleMap (+) c (unCurrentLocation b)

awayFromWalls :: Grid -> Coordinate -> Int
awayFromWalls (Grid (maxX, maxY) _ _) (x,y) = min (afwSingleAxis maxX x) (afwSingleAxis maxY y)

afwSingleAxis :: Int -> Int -> Int
afwSingleAxis max x 
  | x <  (max `div` 2) = x + 1
  | x == (max `div` 2) = x + (max `mod` 2)
  | x >  (max `div` 2) = max - x
