module DriveBikeSpec (spec) where

import Traze.Internal.GameLogic
import Traze.Internal.GameTypes
import TestData

import Test.Hspec

spec :: Spec
spec =
    describe "driveBike: driving a single bike" $ do
        it "turn bike1 to the north" $ do
             driveBike bike1 1 (Steer N) `shouldBe` ( Bike 1 N (2,2) [ (2,1), (1,1) , (0,1) ] )

        it "drive bike1 straight" $ do
             driveBike bike1 1 Straight `shouldBe` ( Bike 1 E (3,1) [ (2,1), (1,1) , (0,1) ] )

        it "turn bike1 to the south" $ do
             driveBike bike1 1 (Steer S) `shouldBe` ( Bike 1 S (2,0) [ (2,1), (1,1) , (0,1) ] )

        it "drive bike2 straight" $ do
             driveBike bike2 1 Straight `shouldBe` ( Bike 2 N (2,8) [ (2,7), (2,6) , (2,5) , (2,4) ] )

        it "turn bike2 to the west" $ do
             driveBike bike2 1 (Steer W) `shouldBe` ( Bike 2 W (1,7) [ (2,7), (2,6) , (2,5) , (2,4) ] )

        it "turn bike2 to the east" $ do
             driveBike bike2 1 (Steer E) `shouldBe` ( Bike 2 E (3,7) [ (2,7), (2,6) , (2,5) , (2,4) ] )
