module GameTypes where

import SpawnQueue

-- A coordinate on the grid
type Coordinate = (Int, Int)

-- gridsize (xMax, yMax)
type GridSize = (Int, Int)

data Course = N | E | W | S
    deriving (Show, Eq)

-- the unPlayers id number
type Player = Int

data Bike = Bike {
    unPlayer :: Player,
    unCourse :: Course,
    unCurrentLocation :: Coordinate,
    unTrail :: [Coordinate]
} deriving (Show, Eq)

type Trail = [Coordinate]

data Command = MoveCommand Player Move
             | Quit Player
-- a move of a unPlayer on the grid
data Move = Steer Course
          | Straight

data Grid = Grid {
    unGridSize :: GridSize
   ,unBikes    :: [Bike]
   ,unQueue    :: [QueueItem Bike]
} deriving (Show, Eq)

-- ticks to start playing
type TimeToLive = Int

data Death = Suicide Player
           --     Killer  Casulty
           | Frag Player  Player
           | Collision Player Player
    deriving (Show, Eq)


