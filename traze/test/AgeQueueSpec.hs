module AgeQueueSpec (spec) where

import Traze.Internal.SpawnQueue

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "ageQueue" $ do
        it "age out two" $ do
            ageQueue [QueueItem 1 'a', QueueItem 3 'b', QueueItem 1 'c'] `shouldBe` [ QueueItem 2 'b']
        it "age out none" $ do
            ageQueue [QueueItem 3 'a', QueueItem 2 'b'] `shouldBe` [ QueueItem 2 'a', QueueItem 1 'b']
        it "remove negative ttl" $ do
            ageQueue [QueueItem 0 'a', QueueItem (-1) 'b'] `shouldBe` []
        it "empty list" $ do
            ageQueue emptyList `shouldBe` []
            where emptyList :: [QueueItem Int]
                  emptyList = []

