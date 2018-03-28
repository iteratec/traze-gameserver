module SpawnPlayerSpec (spec) where

import GameTypes
import GameLogic
import SpawnPlayer

import TestData

import Test.HUnit
import Test.Hspec

spec :: Spec
spec = 
    describe "spawn player" $ do
        it "spawn second player on grid" $ do
            let (grid', Just newBike) = (spawnPlayer grid1)
            assertEqual "still one bike after spawn " 1 $ length (unBikes grid')
            assertEqual "one player queued after spawn" 1 $ length (unQueue grid')
            assertBool "spawned players location is not on a trail" $ not $ ((unCurrentLocation newBike) `elem` allBikeTrailCords (unBikes grid'))
            let (grid'', deaths) = play grid' [(MoveCommand 1 (Steer W))]
            assertEqual "after first move player 1 didn't die" 0 $ length deaths
            assertEqual "the queued player didn't move so he's not on the grid" 1 $ length (unBikes grid'')
            assertEqual "the queued player didn't move so he's still queued" 1 $ length (unQueue grid'')
            let (grid''', deaths') = play grid'' [(MoveCommand 1 (Steer S)), (MoveCommand 2 (Steer W))]
            assertEqual "after move 2 moving nobody died" 0 $ length deaths'
            assertEqual "after moving there are two bikes on grid" 2 $ length (unBikes grid''')
            assertEqual "the queued player spawned so there is an empty queue" 0 $ length (unQueue grid''')
