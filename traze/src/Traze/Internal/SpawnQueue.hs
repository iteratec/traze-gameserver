{-|
Module      : SpawnQueue
Description : waiting queue
Copyright   : (c) Benjamin Brunzel, 2018
License     : BSD3
Maintainer  : benjamin.brunzel@gmail.com

This module provides the implementation for a 
waiting queue with a decaying time to live.
-}

module Traze.Internal.SpawnQueue where

-- | item of a decaying queue
data QueueItem a = QueueItem 
                   TimeToLive -- ^ time until dacaying out of the queue
                   a          -- ^ value that should be helt in the queue
  deriving (Show, Eq)

-- | Time to live; game ticks until a join request decays out of the queue
type TimeToLive = Int

-- | traze default time to live
ttl :: TimeToLive
ttl = 40

-- | create a queue item of a given type
enQueue :: a -> QueueItem a
enQueue a = QueueItem ttl a

-- | retreve the value of a queue item
retrieveQueueItem :: QueueItem a -> a
retrieveQueueItem (QueueItem _ a) = a

-- | retreve the remaining time to live
retrieveQueueTtl :: QueueItem a -> TimeToLive
retrieveQueueTtl (QueueItem t _) = t

-- | decrese the time to live in a queue deleting an entry if @ttl <= 0@
ageQueue :: [QueueItem a] -> [QueueItem a]
ageQueue qs = filter (not . (<=0) . retrieveQueueTtl) $ map decTtl qs

-- | decrement the time to live of a queue item
decTtl :: QueueItem a -> QueueItem a
decTtl (QueueItem t a) = if t > 0
  then QueueItem (t-1) a
  else QueueItem  0    a

