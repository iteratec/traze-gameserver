module HackaTron where

import Data.Either

-- A coordinate on the grid
type Coordinate = (Int, Int)

-- gridsize (xMax, yMax)
type GridSize = (Int, Int)

data Course = N | E | W | S
    deriving (Show, Eq)

-- the players id number
type Player = Int

data Bike = Bike {
    player :: Player,
    course :: Course,
    currLocation :: Coordinate,
    trail :: [Coordinate]
} deriving (Show, Eq)

type Trail = [Coordinate]

data Command = MoveCommand Player Move
             | Quit Player
-- a move of a player on the grid
data Move = Steer Turn 
          | Straight

data Turn = TurnLeft | TurnRight

data Grid = Grid GridSize [Bike]

data Death = Suicide Player
           | Frag Player Player
           | Collision Player Player 

drive :: Grid -> Bike -> Move -> Either Death Bike
drive = undefined

isValidMove :: Grid -> Bike -> Move -> Bool
isValidMove = undefined

driveBike :: Bike -> Move -> Bike
driveBike b m = Bike (player b) (newCourse m) newLocation newTrail
    where newCourse (Steer t)  = turn t $ course b
          newCourse Straight = course b
          
          newLocation = stepCoordinate (newCourse m) (currLocation b)
          newTrail = (currLocation b) : (trail b)

tupleApply :: (a -> c, b -> d) -> (a, b) -> (c, d)
tupleApply (f, g) (a, b) = (f a, g b)

stepCoordinate :: Course -> Coordinate -> Coordinate
stepCoordinate N = tupleApply (id        , (+1)      )
stepCoordinate E = tupleApply ((+1)      , id        )
stepCoordinate W = tupleApply (subtract 1, id        )
stepCoordinate S = tupleApply (id        , subtract 1)


turn :: Turn -> Course -> Course

turn TurnLeft N = W
turn TurnLeft E = N
turn TurnLeft S = E
turn TurnLeft W = S

turn TurnRight N = E
turn TurnRight E = S
turn TurnRight S = W
turn TurnRight W = N

