module PopFromQueueSpec (spec) where

import Traze.Internal.GameTypes
import Traze.Internal.GameLogic
import Traze.Internal.SpawnQueue

import TestData

import Test.Hspec
import Test.HUnit

spec :: Spec
spec =
    describe "popFromQueue" $ do
        it "pop one item" $ do
            let (queue, bikes) = popFromQueue [(QueueItem 1 bike1)] [(MoveCommand 1 Straight)]
            assertEqual "queue is empty after pop" 0 $ length queue
            assertEqual "bike has spawned after pop" 1 $ length bikes
        it "don't pop item" $ do
            let (queue, bikes) = popFromQueue [(QueueItem 1 bike1)] [(MoveCommand 2 Straight)]
            assertEqual "queue is not empty after pop" 1 $ length queue
            assertEqual "bike has not spawned after pop" 0 $ length bikes
        it "don't pop item" $ do
            let (queue, bikes) = popFromQueue [(QueueItem 1 bike1),(QueueItem 1 bike2), QueueItem 1 bike3] [(MoveCommand 2 Straight)]
            assertEqual "queue is not empty after pop" 2 $ length queue
            assertEqual "bike has not spawned after pop" 1 $ length bikes

