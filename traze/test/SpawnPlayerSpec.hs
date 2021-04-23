module SpawnPlayerSpec (spec) where

import Traze.Internal.GameTypes
import Traze.Internal.GameLogic
import Traze.Internal.SpawnPlayer

import TestData

import Test.HUnit
import Test.Hspec

spec :: Spec
spec =
    describe "spawn player" $ do
        it "no spawning in running game" $ do
            let (grid', Nothing) = (spawnPlayer grid2)
            assertEqual "still one bike after spawn " 2 $ length (unBikes grid')
            assertEqual "queue is always empty" 0 $ length (unQueue grid')

        it "spawn players on grid" $ do
            let (grid', Just newBike) = (spawnPlayer grid4)
            print grid'
            assertEqual "spawn player " 3 $ length (unBikes grid')
            assertEqual "queue is always empty" 0 $ length (unQueue grid')
