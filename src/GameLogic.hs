module GameLogic where

import GameTypes
import SpawnQueue
import Tuple

import Data.Either (lefts, rights)
import Data.List (find)

import GHC.Exts (groupWith, sortWith)

-- | The 'play' function plays one round. It takes a list of 
-- Commands and generates the next game state as well as 
play :: Grid           -- ^ The current game state
    -> [Command]       -- ^ The list of commands given by the players for this turn
    -> (Grid, [Death]) -- ^ The resulting game state, the list of deaths resulting from this turn
play g cs = ((Grid (unGridSize g) bikes' queue'), deaths)
    where bikes' = rights $ executedCommands
          deaths = lefts $ executedCommands
          executedCommands = map (execCommand g) $ 
              map (addStraightCommands cs) ((unBikes g) ++ joinedBikes)
          (popedQueue, joinedBikes) = popFromQueue (unQueue g) cs
          queue' = ageQueue popedQueue

-- | 
popFromQueue :: [QueueItem Bike] -> [Command] -> ([QueueItem Bike], [Bike])
popFromQueue qs cs = (queue', bikes)
    where bikes = filter (hasCommand cs) (map unQueueItem qs)
          queue' = filter (not . (findPlayerIdInQueue bikes)) qs
          hasCommand :: [Command] -> Bike -> Bool
          hasCommand coms b = (unPlayerId b) `elem` (map getCommandPlayerId coms)
          findPlayerIdInQueue :: [Bike] -> QueueItem Bike -> Bool
          findPlayerIdInQueue bs item = (unPlayerId (unQueueItem item)) `elem` (map unPlayerId bs)

-- make sure all bikes on the grid go straight if no command given
addStraightCommands :: [Command] -> Bike -> Command
addStraightCommands cs b = case command of
    Nothing -> MoveCommand (unPlayerId b) Straight
    Just c  -> c
    where command :: Maybe Command
          command = find (\c -> getCommandPlayerId c == (unPlayerId b)) cs

getCommandPlayerId :: Command -> PlayerId
getCommandPlayerId (MoveCommand p _) = p
getCommandPlayerId (Quit p) = p

removeDuplicateCommands :: [Command] -> [Command]
removeDuplicateCommands = removeDuplicates (getCommandPlayerId)

removeDuplicates :: Ord b => (a -> b) -> [a] -> [a]
removeDuplicates f = map (foldr1 (\_ a -> a)) . (groupWith f) . (sortWith f)

execCommand :: Grid -> Command -> Either Death Bike
execCommand _ (Quit p) = Left $ Suicide p
execCommand g (MoveCommand p m) = case (getBikeForPlayerId g p) of
    (Just b)  -> drive g b m
    (Nothing) -> Left $ Suicide p

getBikeForPlayerId :: Grid -> PlayerId -> Maybe Bike
getBikeForPlayerId (Grid _ bs q) p =
    let allBikes = bs ++ (map unQueueItem q) in
        (find (\a -> p == unPlayerId a) allBikes)

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
              then Just (Suicide (unPlayerId b))
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
    Just f -> Just $ getDeath (unPlayerId f) (unPlayerId b)

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

getFirstJust :: [Maybe a] -> Maybe a
getFirstJust []           = Nothing
getFirstJust [x]          = x
getFirstJust ((Just x):_) = Just x
getFirstJust (_:xs)       = getFirstJust xs

-- execute a single move
driveBike :: Bike -> Move -> Bike
driveBike b m = Bike (unPlayerId b) (newCourse (unCourse b) m) newLocation newTrail
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

