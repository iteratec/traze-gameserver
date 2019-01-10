{-# LANGUAGE DeriveGeneric #-}
module Traze.Internal.GameTypes where

import Traze.Internal.SpawnQueue
import GHC.Generics
import Data.Aeson

-- A coordinate on the grid
type Coordinate = (Int, Int)

-- gridsize (xMax, yMax)
type GridSize = (Int, Int)

data Course = N | E | W | S
    deriving (Generic, Show, Eq)

instance ToJSON Course where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Course

-- the unPlayers id number
type PlayerId = Int

data Bike = Bike {
    bikePlayerId :: PlayerId,
    unCourse :: Course,
    unCurrentLocation :: Coordinate,
    unTrail :: Trail
} deriving (Show, Eq)

type Trail = [Coordinate]

data Command = MoveCommand PlayerId Move
             | Quit PlayerId
    deriving (Show, Eq)
-- a move of a unPlayer on the grid
data Move = Steer Course
          | Straight
    deriving (Show, Eq)

data Grid = Grid {
    unGridSize :: GridSize
   ,unBikes    :: [Bike]
   ,unQueue    :: [QueueItem Bike]
} deriving (Show, Eq)

type TimeToLive = Int

data Death = Suicide PlayerId
           --     Killer  Casulty
           | Frag PlayerId  PlayerId
           | Collision PlayerId PlayerId
    deriving (Show, Eq)

getCommandPlayerId :: Command -> PlayerId
getCommandPlayerId (MoveCommand p _) = p
getCommandPlayerId (Quit p) = p

