{-# LANGUAGE OverloadedStrings #-}

module HackaTron where

import qualified Data.Text as Text
import Control.Lens (set, ix)
import Data.Char (chr, ord)

gridSizeX :: Int
gridSizeX = 5

gridSizeY :: Int
gridSizeY = 5

data Marking =  Empty | Occupied Player
    deriving (Show, Eq)

type Grid = [[Marking]]

type Coordinate = (Int, Int)

data Direction = N | E | W | S
    deriving (Show, Eq)

data Player = Player {
    _id  :: Int,
    name :: Text.Text,
    bike :: Bike
}
  deriving (Show, Eq)

type Bike = [Coordinate]

data Game = Game {
    grid    :: Grid,
    players :: [Player]
}
    deriving (Show,Eq)

emptyGrid :: Int -> Int -> Grid
emptyGrid x y = replicate x $ replicate y Empty

-- renders a Grid into a String
printGrid :: Grid -> String
printGrid = foldr inner "" 
  where inner :: [Marking] -> [Char] -> [Char]
        inner m s = (foldMarking m) ++ "\n" ++ s
        
        foldMarking :: Foldable t => t Marking -> [Char]
        foldMarking = foldr ((:) . printMarking) []

        printMarking :: Marking -> Char
        printMarking Empty = ' '
        printMarking (Occupied p) = chr $ ord '0' + (mod (_id p) 10)

setCoordinate :: Marking -> Coordinate -> Grid -> Grid
setCoordinate mark (i, j) grid = set (ix i . ix j) mark grid

validCoordinate :: Coordinate -> Bool
validCoordinate (i, j) = i > 0 && j > 0 && i < gridSizeX && j < gridSizeY

tupleApply :: (a -> c, b -> d) -> (a, b) -> (c, d)
tupleApply (f, g) (a, b) = (f a, g b)

stepCoordinate :: Direction -> Coordinate -> Coordinate
stepCoordinate N = tupleApply (subtract 1, id        )
stepCoordinate E = tupleApply (id        , (+1)      )
stepCoordinate W = tupleApply (id        , subtract 1)
stepCoordinate S = tupleApply ((+1)      , id        )

