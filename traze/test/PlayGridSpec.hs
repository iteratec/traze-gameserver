module PlayGridSpec (spec) where

import Traze.Internal.GameTypes
import Traze.Internal.GameLogic

import TestData

import Test.Hspec

spec :: Spec
spec =
-- 1O22
-- 1XXX
-- XXXX

    describe "play" $ do
        it "both dont react (straight): 2 crashes in 1" $ do
             play grid2 [] `shouldBe` (Grid (4,4) [(Bike 1 E (2,3) [(1,3), (0,3), (0,2), (0,1)])] [] 3, [Frag 1 2])
        it "1 steers left: 1 kisses the wall, two crashes in 1" $ do
              play grid2 [(MoveCommand 1 (Steer N))] `shouldBe` (Grid (4,4) [] [] 3, [Suicide 1, Frag 1 2])
        it "1 steers right: each runs into the other" $ do
             play grid2 [(MoveCommand 1 (Steer S))] `shouldBe` (Grid (4,4) [] [] 3, [Frag 2 1, Frag 1 2])
        it "1 steers right, 2 steers left: 2 is fine but 1 crashes in them" $ do
             play grid2 [(MoveCommand 1 (Steer S)),(MoveCommand 2 (Steer S))] `shouldBe` (Grid (4,4) [Bike 2 S (1,1) [(1,2),(2,2), (3,2), (3,3)]] [] 3 , [Frag 2 1])
-- XXX
-- 1X2
-- XXX

        it "both go straight, reaching the same sqare at the same time" $ do
            play grid3 [] `shouldBe` (Grid (3,3) [] [] 4, [Suicide 1, Suicide 2])

        it "non colliding bikes" $ do
            hasCollided (Bike 1 N (1,1) []) [(Bike 1 N (1,1) []), (Bike 2 N (1,2) [])] `shouldBe` False
        it "colliding bikes" $ do
            hasCollided (Bike 1 N (1,1) []) [(Bike 1 N (1,1) []), (Bike 2 N (1,1) [])] `shouldBe` True

        it "no colliding Bikes" $ do
            filterCollisions [(Bike 1 N (1,1) []), (Bike 2 N (1,2) [])] `shouldBe` ([(Bike 1 N (1,1) []), (Bike 2 N (1,2) [])], [])
        it "two colliding Bikes" $ do
            filterCollisions [(Bike 1 N (1,2) []), (Bike 2 N (1,2) [])] `shouldBe` ([], [(Suicide 1), (Suicide 2)])
