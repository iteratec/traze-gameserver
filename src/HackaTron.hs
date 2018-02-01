module HackaTron where

import Data.Either (lefts, rights)
import Data.List (find)
import Data.Maybe (fromJust)

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
data Move = Steer Turn 
          | Straight

data Turn = TurnLeft | TurnRight

data Grid = Grid {
    unGridSize :: GridSize
   ,unBikes    :: [Bike] 
   ,unQueue    :: [QueueItem Bike]
} deriving (Show, Eq)

-- ticks to start playing
type TimeToLive = Int

data QueueItem a = QueueItem TimeToLive a
  deriving (Show, Eq)

unQueueBike :: QueueItem Bike -> Bike
unQueueBike (QueueItem _ b) = b

ttl :: TimeToLive
ttl = 50

enQueue :: a -> QueueItem a
enQueue a = QueueItem ttl a

retrieveQueueItem :: QueueItem a -> a
retrieveQueueItem (QueueItem _ a) = a

data Death = Suicide Player
           --     Killer  Casulty
           | Frag Player  Player
           | Collision Player Player 
    deriving (Show, Eq)

-- | The 'play' function plays one round. It takes a list of 
-- Commands and generates the next game state as well as 
play :: Grid           -- ^ The current game state
    -> [Command]       -- ^ The list of commands given by the players for this turn
    -> (Grid, [Death]) -- ^ The resulting game state, the list of deaths resulting from this turn
play g cs = ((Grid (unGridSize g) bikes' queue'), deaths)
    where bikes' = rights $ executedCommands
          deaths = lefts $ executedCommands
          executedCommands = map (execCommand g) $ map (addStraightCommands cs) ((unBikes g) ++ joinedBikes)
          (queue', joinedBikes) = popFromQueue (unQueue g) cs

-- | 
popFromQueue :: [QueueItem Bike] -> [Command] -> ([QueueItem Bike], [Bike])
popFromQueue qs cs = (queue', bikes)
    where bikes = filter (hasCommand cs) (map unQueueBike qs)
          queue' = filter (findPlayerInQueue bikes) qs 
          hasCommand :: [Command] -> Bike -> Bool
          hasCommand coms b = (unPlayer b) `elem` (map getCommandPlayer coms)
          findPlayerInQueue :: [Bike] -> QueueItem Bike -> Bool
          findPlayerInQueue bs item = (unPlayer (unQueueBike item)) `elem` (map unPlayer bs)

-- make sure all bikes on the grid go straight if no command given
addStraightCommands :: [Command] -> Bike -> Command
addStraightCommands cs b = case command of
    Nothing -> MoveCommand (unPlayer b) Straight
    Just c  -> c
    where command :: Maybe Command
          command = find (\c -> getCommandPlayer c == (unPlayer b)) cs

getCommandPlayer :: Command -> Player
getCommandPlayer (MoveCommand p _) = p
getCommandPlayer (Quit p) = p

execCommand :: Grid -> Command -> Either Death Bike
execCommand _ (Quit p) = Left $ Suicide p
execCommand g (MoveCommand p m) = case (getBikeForPlayer g p) of
    (Just b) -> drive g b m
    (Nothing)  -> Left $ Suicide p

getBikeForPlayer :: Grid -> Player -> Maybe Bike
getBikeForPlayer (Grid _ bs _) p = case (filter (\a -> p == unPlayer a) bs) of
    []    -> Nothing
    [x]   -> Just x
    (x:_) -> Just x

drive :: Grid -> Bike -> Move -> Either Death Bike
drive g b m = case death of
    Just d -> Left d
    Nothing -> Right $ driveBike b m

    where newCord = stepCoordinate unCourse' (unCurrentLocation b)
          unCourse' = newCourse (unCourse b) m
          
          death = getFirstJust $ (getWallKiss $ unGridSize g) : (getFrag g b m) : []

          -- get death if out of grid
          getWallKiss :: GridSize -> Maybe Death
          getWallKiss gridSize = if (isOutOfBounds newCord gridSize)
              then Just (Suicide (unPlayer b))
              else Nothing

isOutOfBounds :: Coordinate -> Coordinate -> Bool
isOutOfBounds (x1,y1) (x2, y2)
    | x1 >= x2  = True
    | y1 >= y2  = True
    | x1 < 0    = True
    | y1 < 0    = True
    | otherwise = False

-- returns the death caused by hitting a unTrail
getFrag :: Grid -> Bike -> Move -> Maybe Death
getFrag (Grid _ bikes _) b m = case fragger of
    Nothing -> Nothing
    Just f -> Just $ getDeath (unPlayer f) (unPlayer b)

    where fragger :: Maybe Bike
          fragger = getFirstJust $ fmap (droveInTrail $ newCord) bikes

          unCourse' = newCourse (unCourse b) m
          newCord = stepCoordinate unCourse' (unCurrentLocation b)

          droveInTrail :: Coordinate -> Bike -> Maybe Bike
          droveInTrail c bike =  if c `elem` ( unCurrentLocation bike : (unTrail bike))
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
driveBike b m = Bike (unPlayer b) (newCourse (unCourse b) m) newLocation newTrail
    where newLocation = stepCoordinate (newCourse (unCourse b) m) (unCurrentLocation b)
          newTrail = (unCurrentLocation b) : (unTrail b)

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

spawnPlayer :: Grid -> (Grid, Maybe Bike)
spawnPlayer g = case spawnCoord of
    Nothing   -> (g, Nothing)
    Just _ -> (Grid (unGridSize g) (unBikes g) ((enQueue b) : (unQueue g)), Just b)
    where b = Bike (newPlayerId (map (unPlayer) $ unBikes g)) N (fromJust spawnCoord) []
          spawnCoord = getSpawnCoord g

newPlayerId :: [Player] -> Player
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
spawnScore g c = (awayFromAnyBike g c) + (awayFromWalls g c)

allFreeCoords :: Grid -> [Coordinate]
allFreeCoords (Grid gs bs q) = filter (not . (flip elem $ allBikeTrailCords bs ++ queuedCoords)) (allCoords gs)
    where queuedCoords = (map (unCurrentLocation . retrieveQueueItem) q)

allBikeTrailCords :: [Bike] -> [Coordinate]
allBikeTrailCords = concatMap (unTrail)

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
awayFromWalls (Grid gs _ _) c = tupleFold min $ tupleMap (-) gs c

tupleDo :: (a -> b) -> (a, a) -> (b, b)
tupleDo f (x, y) = (f x, f y)

tupleFold :: (a -> b -> c) -> (a, b) -> c
tupleFold f (x, y) = f x y

tupleMap :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
tupleMap f (x1, y1) (x2, y2) = (x1 `f` x2, y1 `f` y2)

tupleApply :: (a -> c, b -> d) -> (a, b) -> (c, d)
tupleApply (f, g) (a, b) = (f a, g b)

