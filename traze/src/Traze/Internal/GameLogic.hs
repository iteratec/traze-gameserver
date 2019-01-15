module Traze.Internal.GameLogic
  ( play
  , getTiles
  -- below should not be reexported
  , isOnGrid
  , isInQueue
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

-- | playing one game round. Takes a list of player
--   commands and generates the next game state as well as a 
--   list of possible deaths as a result of the player actions

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


-- | returns true if there is a player with playerId on the grid

isOnGrid :: Grid -> PlayerId -> Bool
isOnGrid g pid = elem pid (fmap bikePlayerId (unBikes g))

-- | returns true if there is a player with playerId in the grid queue
isInQueue :: Grid -> PlayerId -> Bool
isInQueue g pid = elem pid (fmap (bikePlayerId . retrieveQueueItem) (unQueue g))

-- | pops a bike from the queue if there is a command for it
popFromQueue :: [QueueItem Bike] -> [Command] -> ([QueueItem Bike], [Bike])
popFromQueue qs cs = (queue', bikes)
    where bikes = filter (hasCommand cs) (map retrieveQueueItem qs)
          queue' = filter (not . (findPlayerIdInQueue bikes)) qs
          hasCommand :: [Command] -> Bike -> Bool
          hasCommand coms b = (bikePlayerId b) `elem` (map getCommandPlayerId coms)
          findPlayerIdInQueue :: [Bike] -> QueueItem Bike -> Bool
          findPlayerIdInQueue bs item = (bikePlayerId (retrieveQueueItem item)) `elem` (map bikePlayerId bs)

-- | identifies all colisions for a list of bikes and returns
--   the resulting list of bikes as well as a list of deaths.
--   Note: this is only looking at Colisions not Frags or Suicides.
filterCollisions :: [Bike] -> ([Bike], [Death])
filterCollisions bs = (bikes', deaths)
    where collidedBikes = filter (\b -> hasCollided b bs) bs
          bikes' = filter (\b -> not $ b `elem` collidedBikes) bs 
          deaths = map (\b -> Suicide $ bikePlayerId b) collidedBikes

-- | returns true if a bike collides with a bike in a given list.
hasCollided :: Bike -> [Bike] -> Bool
hasCollided (Bike pid _ loc _) bs = length (filter (\(Bike otherPid _ otherLoc _) -> pid /= otherPid && loc == otherLoc) bs) > 0

-- | make sure all bikes on the grid go straight if no command given
addStraightCommands :: [Command] -> Bike -> Command
addStraightCommands cs b = case command of
    Nothing -> MoveCommand (bikePlayerId b) Straight
    Just c  -> c
    where command :: Maybe Command
          command = find (\c -> getCommandPlayerId c == (bikePlayerId b)) cs

-- | execute a player command on a grid resulting
--   in a death or an updated bike
execCommand :: Grid -> Command -> Either Death Bike
execCommand _ (Quit p) = Left $ Suicide p
execCommand g (MoveCommand p m) = case (getBikeForPlayerId g p) of
    (Just b)  -> drive g b m
    (Nothing) -> Left $ Suicide p

-- | find the bike for a playerId
getBikeForPlayerId :: Grid -> PlayerId -> Maybe Bike
getBikeForPlayerId (Grid _ bs q) p =
    let allBikes = bs ++ (map retrieveQueueItem q) in
        (find (\a -> p == bikePlayerId a) allBikes)

-- | drive a bike on the grid for one step
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

-- | returns true if coordinate 1 is outside of coordinate 2
isOutOfBounds :: Coordinate -> Coordinate -> Bool
isOutOfBounds (x1,y1) (x2, y2)
    | x1 >= x2  = True
    | y1 >= y2  = True
    | x1 < 0    = True
    | y1 < 0    = True
    | otherwise = False

-- | returns the death caused by hitting an trail
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

-- | update a bikes location as well as trail based on a given move
driveBike :: Bike -> Move -> Bike
driveBike b m = Bike (bikePlayerId b) (newCourse (unCourse b) m) newLocation newTrail
    where newLocation = stepCoordinate (newCourse (unCourse b) m) (unCurrentLocation b)
          newTrail = (unCurrentLocation b) : (unTrail b)

-- | get the new cource based on the old course and a given move
newCourse :: Course -> Move -> Course
newCourse c Straight  = c
newCourse _ (Steer t) = t

-- | update the location based on the course
stepCoordinate :: Course -> Coordinate -> Coordinate
stepCoordinate N = tupleApply (id        , (+1)      )
stepCoordinate E = tupleApply ((+1)      , id        )
stepCoordinate W = tupleApply (subtract 1, id        )
stepCoordinate S = tupleApply (id        , subtract 1)

-- | get the playerId at a certain coordinate
getPosPlayerId :: [Bike] -> Coordinate -> PlayerId
getPosPlayerId bs c = fromMaybe 0 $ getFirstJust $ map (getPid c) bs
  where 
    getPid :: Coordinate -> Bike -> Maybe PlayerId
    getPid coord b
        | coord == (unCurrentLocation b) = Just (bikePlayerId b)
        | coord `elem` (unTrail b) = Just (bikePlayerId b)
        | otherwise = Nothing

-- | returns an array of player ids based on the trails on a given grid.
--   You may see this as a "rendered" representation of the grid.

getTiles :: Grid -> [[Int]]
getTiles g = (map . map) (getPosPlayerId gridBikes) (getGridCoords gs)
  where (Grid gs gridBikes _) = g

        getGridCoords :: GridSize -> [[Coordinate]]
        getGridCoords (maxX, maxY) =
          map (getLineCoords (maxX, maxY)) $ [0..(maxX-1)]

        getLineCoords :: GridSize -> Int -> [Coordinate]
        getLineCoords (maxY,_) x = [(x,y) | y <- [0..(maxY-1)]]

