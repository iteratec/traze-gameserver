{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : GameTypes
Description : game model
Copyright   : (c) Benjamin Brunzel, 2018
License     : BSD3
Maintainer  : benjamin.brunzel@gmail.com

This module contains the game objects in the
traze game domain.
-}
module Traze.Internal.GameTypes where

import Traze.Internal.SpawnQueue
import GHC.Generics
import Data.Aeson

-- | A coordinate on the grid (x, y)
type Coordinate = (Int, Int)

-- | size of the grid (xMax, yMax)
type GridSize = (Int, Int)

-- | The course in which a bike can be heading
data Course = N   -- ^ North
            | E   -- ^ East
            | W   -- ^ South
            | S   -- ^ West
    deriving (Generic, Show, Eq)

instance ToJSON Course where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Course

-- | a unique identifing number for a player
type PlayerId = Int

-- | the speed at which a bike rides
type Speed = Int

-- | the trail of a bike on the grid
type Trail = [Coordinate]

-- | a bike that is currently alive on the grid
data Bike = Bike {
    bikePlayerId :: PlayerId,         -- ^ the player id of the player controlling this bike
    unCourse :: Course,               -- ^ the course the bike is heading towards
    unCurrentLocation :: Coordinate,  -- ^ the current location on the grid
    unTrail :: Trail                  -- ^ the bikes trail on the grid
} deriving (Show, Eq)

-- | a move of a Player on the grid
data Move = Steer Course   -- ^ steer towards a given course
          | Straight       -- ^ continue going straight
    deriving (Show, Eq)

-- | a players command for his bike during a time frame
data Command = MoveCommand PlayerId Move -- ^ move for the bike of player with id
             | Quit PlayerId             -- ^ quit the game
    deriving (Show, Eq)

-- | representation of a game grid at a time
data Grid = Grid {
    unGridSize :: GridSize           -- ^ physical limits of the grid
   ,unBikes    :: [Bike]             -- ^ bikes that are active on the grid
   ,unQueue    :: [QueueItem Bike]   -- ^ bikes that are waiting to be spawned on the grid
   ,unRound     :: Round
} deriving (Show, Eq)

data Round = Round {
    unTick :: Int,
    unGameStartTick :: Int
} deriving (Show, Eq)

type Killer = PlayerId
type Casulty = PlayerId

-- | death of a player on the grid
data Death = Suicide PlayerId            -- ^ suicide of player with ID.
                                         --   May be caused by driving against the grid wall
                                         --   or by hitting the own trail.
                                         --
           | Frag Killer  Casulty        -- ^ the casulty drove into the killers trail.
           | Collision Casulty Casulty   -- ^ both casulties drove onto the same coodinate
                                         --   at the same time
    deriving (Show, Eq)

getCommandPlayerId :: Command -> PlayerId
getCommandPlayerId (MoveCommand p _) = p
getCommandPlayerId (Quit p) = p
