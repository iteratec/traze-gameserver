module SpawnQueue where

data QueueItem a = QueueItem TimeToLive a
  deriving (Show, Eq)

-- ticks to start playing
type TimeToLive = Int

unQueueItem :: QueueItem a -> a
unQueueItem (QueueItem _ b) = b

ttl :: TimeToLive
ttl = 40

enQueue :: a -> QueueItem a
enQueue a = QueueItem ttl a

retrieveQueueItem :: QueueItem a -> a
retrieveQueueItem (QueueItem _ a) = a

retrieveQueueTtl :: QueueItem a -> TimeToLive
retrieveQueueTtl (QueueItem t _) = t

ageQueue :: [QueueItem a] -> [QueueItem a]
ageQueue qs = filter (not . (<=0) . retrieveQueueTtl) $ map decTtl qs

decTtl :: QueueItem a -> QueueItem a
decTtl (QueueItem t a) = QueueItem (t-1) a

getFirstJust :: [Maybe a] -> Maybe a
getFirstJust []           = Nothing
getFirstJust [x]          = x
getFirstJust ((Just x):_) = Just x
getFirstJust (_:xs)       = getFirstJust xs
