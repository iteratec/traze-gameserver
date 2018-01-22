module HackaTron where

import Data.Either (lefts, rights)
import Data.List (find)

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
           --     Casulty Killer
           | Frag Player  Player
           | Collision Player Player 

play :: Grid -> [Command] -> (Grid, [Death])
play g cs = ((Grid (getGridSize g) bikes'), deaths)
    where bikes' = rights $ executedCommands
          deaths = lefts $ executedCommands
          executedCommands = map (execCommand g) $ map (addStraightCommands cs) (getGridBikes g)

-- make sure all bikes on the grid go straight if no command given
addStraightCommands :: [Command] -> Bike -> Command
addStraightCommands cs b = case command of
    Nothing -> MoveCommand (player b) Straight
    Just c  -> c
    where command :: Maybe Command
          command = find (\c -> getCommandPlayer c == (player b)) cs

getCommandPlayer :: Command -> Player
getCommandPlayer (MoveCommand p _) = p
getCommandPlayer (Quit p) = p

execCommand :: Grid -> Command -> Either Death Bike
execCommand _ (Quit p) = Left $ Suicide p
execCommand g (MoveCommand p m) = case (getBikeForPlayer g p) of
    (Just b) -> drive g b m
    (Nothing)  -> Left $ Suicide p

getBikeForPlayer :: Grid -> Player -> Maybe Bike
getBikeForPlayer (Grid _ bs) p = case (filter (\a -> p == player a) bs) of
    []    -> Nothing
    [x]   -> Just x
    (x:_) -> Just x

getGridSize :: Grid -> GridSize
getGridSize (Grid gs _ ) = gs

getGridBikes :: Grid -> [Bike]
getGridBikes (Grid _ bs) = bs

drive :: Grid -> Bike -> Move -> Either Death Bike
drive g b m = case death of
    Just d -> Left d
    Nothing -> Right $ driveBike b m

    where newCord = stepCoordinate course' (currLocation b)
          course' = newCourse (course b) m
          
          death = getFirstJust $ (getWallKiss $ getGridSize g) : (getFrag g b m) : []

          -- get death if out of grid
          getWallKiss :: GridSize -> Maybe Death
          getWallKiss gridSize = if (newCord < (0,0)) || (newCord >= gridSize)
              then Just (Suicide (player b))
              else Nothing

-- returns the death caused by hitting a trail
getFrag :: Grid -> Bike -> Move -> Maybe Death
getFrag (Grid _ bikes) b m = case fragger of
    Nothing -> Nothing
    Just f -> Just $ getDeath (player f) (player b)

    where fragger :: Maybe Bike
          fragger = getFirstJust $ fmap (droveInTrail $ newCord) bikes

          course' = newCourse (course b) m
          newCord = stepCoordinate course' (currLocation b)

          droveInTrail :: Coordinate -> Bike -> Maybe Bike
          droveInTrail c bike =  if c `elem` (trail bike)
              then Just bike
              else Nothing

          getDeath :: Player -> Player -> Death
          getDeath p killer = if p == killer
              then Suicide p
              else Frag p killer

getFirstJust :: [Maybe a] -> Maybe a
getFirstJust [] = Nothing
getFirstJust [x] = x
getFirstJust ((Just x):_) = Just x
getFirstJust (_:xs) = getFirstJust xs

-- execute a single move
driveBike :: Bike -> Move -> Bike
driveBike b m = Bike (player b) (newCourse (course b) m) newLocation newTrail
    where newLocation = stepCoordinate (newCourse (course b) m) (currLocation b)
          newTrail = (currLocation b) : (trail b)

newCourse :: Course -> Move -> Course
newCourse c Straight   = c
newCourse c (Steer t)  = turn t $ c

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

spawnPlayer :: Grid -> (Grid, Bike)
spawnPlayer = undefined

awayFromBike :: Coordinate -> Bike -> Int
awayFromBike c b = round squareRoot
    where
        squareRoot :: Double
        squareRoot  = sqrt $
            tupleFold (+) $
            tupleDo fromIntegral $
            tupleDo ((flip (^)) (2 :: Int)) $
            tupleDo abs $
            tupleMap (+) c (currLocation b)

awayFromWalls :: Coordinate -> GridSize -> Int
awayFromWalls c gs = tupleFold min $ tupleMap (-) gs c

tupleDo :: (a -> b) -> (a, a) -> (b, b)
tupleDo f (x, y) = (f x, f y)

tupleFold :: (a -> b -> c) -> (a, b) -> c
tupleFold f (x, y) = f x y

tupleMap :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tupleMap f (x1, y1) (x2, y2) = (x1 `f` x2, y1 `f` y2)

tupleApply :: (a -> c, b -> d) -> (a, b) -> (c, d)
tupleApply (f, g) (a, b) = (f a, g b)

