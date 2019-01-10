module Traze.Internal.GameLogic
  ( play
  , getTiles
  -- below should not be reexported
  , popFromQueue
  , hasCollided
  , filterCollisions
  , execCommand
  , driveBike
) where

import Traze.Internal.GameTypes as GT
import Traze.Internal.SpawnQueue
import Traze.Internal.Tuple

import Data.Either (lefts, rights)
import Data.List (find)
import Data.Maybe (fromMaybe)

-- | The 'play' function plays one round. It takes a list of 
-- Commands and generates the next game state as well as 
play :: Grid           -- ^ The current game state
    -> [Command]       -- ^ The list of commands given by the players for this turn
    -> (Grid, [Death]) -- ^ The resulting game state, the list of deaths resulting from this turn
play g cs = ((Grid (unGridSize g) bikes'' queue'), deaths ++ collisionSuicides)
    where (bikes'', collisionSuicides) = filterCollisions bikes'
          bikes' = rights $ executedCommands
          deaths = lefts $ executedCommands
          executedCommands = map (execCommand g) $ 
              map (addStraightCommands cs) ((unBikes g) ++ joinedBikes)
          (popedQueue, joinedBikes) = popFromQueue (unQueue g) cs
          queue' = ageQueue popedQueue

-- | returns an array of player ids based on the  a given grid
getTiles :: Grid -> [[Int]]
getTiles g = (map . map) (getPosPlayerId gridBikes) (getGridCoords gs)
    where (Grid gs gridBikes _) = g

popFromQueue :: [QueueItem Bike] -> [Command] -> ([QueueItem Bike], [Bike])
popFromQueue qs cs = (queue', bikes)
    where bikes = filter (hasCommand cs) (map unQueueItem qs)
          queue' = filter (not . (findPlayerIdInQueue bikes)) qs
          hasCommand :: [Command] -> Bike -> Bool
          hasCommand coms b = (bikePlayerId b) `elem` (map getCommandPlayerId coms)
          findPlayerIdInQueue :: [Bike] -> QueueItem Bike -> Bool
          findPlayerIdInQueue bs item = (bikePlayerId (unQueueItem item)) `elem` (map bikePlayerId bs)

filterCollisions :: [Bike] -> ([Bike], [Death])
filterCollisions bs = (bikes', deaths)
    where collidedBikes = filter (\b -> hasCollided b bs) bs
          bikes' = filter (\b -> not $ b `elem` collidedBikes) bs 
          deaths = map (\b -> Suicide $ bikePlayerId b) collidedBikes

hasCollided :: Bike -> [Bike] -> Bool
hasCollided (Bike pid _ loc _) bs = length (filter (\(Bike otherPid _ otherLoc _) -> pid /= otherPid && loc == otherLoc) bs) > 0

-- make sure all bikes on the grid go straight if no command given
addStraightCommands :: [Command] -> Bike -> Command
addStraightCommands cs b = case command of
    Nothing -> MoveCommand (bikePlayerId b) Straight
    Just c  -> c
    where command :: Maybe Command
          command = find (\c -> getCommandPlayerId c == (bikePlayerId b)) cs

execCommand :: Grid -> Command -> Either Death Bike
execCommand _ (Quit p) = Left $ Suicide p
execCommand g (MoveCommand p m) = case (getBikeForPlayerId g p) of
    (Just b)  -> drive g b m
    (Nothing) -> Left $ Suicide p

getBikeForPlayerId :: Grid -> PlayerId -> Maybe Bike
getBikeForPlayerId (Grid _ bs q) p =
    let allBikes = bs ++ (map unQueueItem q) in
        (find (\a -> p == bikePlayerId a) allBikes)

drive :: Grid -> Bike -> Move -> Either Death Bike
drive g b m = case death of
    Just d  -> Left d
    Nothing -> Right $ driveBike b m

    where newCord = stepCoordinate unCourse' (unCurrentLocation b)
          unCourse' = newCourse (unCourse b) m
          
          death = getFirstJust $ (getWallKiss $ unGridSize g) : (getFrag g b m) : []

          -- get death if out of grid
          getWallKiss :: GridSize -> Maybe Death
          getWallKiss gridSize = if (isOutOfBounds newCord gridSize)
              then Just (Suicide (bikePlayerId b))
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
    Just f -> Just $ getDeath (bikePlayerId f) (bikePlayerId b)

    where fragger :: Maybe Bike
          fragger = getFirstJust $ fmap (droveInTrail $ newCord) bikes

          unCourse' = newCourse (unCourse b) m
          newCord = stepCoordinate unCourse' (unCurrentLocation b)

          droveInTrail :: Coordinate -> Bike -> Maybe Bike
          droveInTrail c bike =  if c `elem` ( unCurrentLocation bike : (unTrail bike))
              then Just bike
              else Nothing

          getDeath :: PlayerId -> PlayerId -> Death
          getDeath p killer = if p == killer
              then Suicide p
              else Frag p killer

-- execute a single move
driveBike :: Bike -> Move -> Bike
driveBike b m = Bike (bikePlayerId b) (newCourse (unCourse b) m) newLocation newTrail
    where newLocation = stepCoordinate (newCourse (unCourse b) m) (unCurrentLocation b)
          newTrail = (unCurrentLocation b) : (unTrail b)

newCourse :: Course -> Move -> Course
newCourse c Straight  = c
newCourse _ (Steer t) = t

stepCoordinate :: Course -> Coordinate -> Coordinate
stepCoordinate N = tupleApply (id        , (+1)      )
stepCoordinate E = tupleApply ((+1)      , id        )
stepCoordinate W = tupleApply (subtract 1, id        )
stepCoordinate S = tupleApply (id        , subtract 1)

getPosPlayerId :: [Bike] -> Coordinate -> PlayerId
getPosPlayerId bs c = fromMaybe 0 $ getFirstJust $ map (getPid c) bs

getPid :: Coordinate -> Bike -> Maybe PlayerId
getPid c b
    | c == (unCurrentLocation b) = Just (bikePlayerId b)
    | c `elem` (unTrail b) = Just (bikePlayerId b)
    | otherwise = Nothing

getGridCoords :: GridSize -> [[Coordinate]]
getGridCoords (maxX, maxY) =
    map (getLineCoords (maxX, maxY)) $ [0..(maxX-1)]

getLineCoords :: GridSize -> Int -> [Coordinate]
getLineCoords (maxY,_) x = [(x,y) | y <- [0..(maxY-1)]]

