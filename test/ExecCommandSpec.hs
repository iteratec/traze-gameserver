module ExecCommandSpec (spec) where

import GameTypes
import GameLogic

import TestData

import Test.Hspec

-- XO1
-- X11
-- XXX

spec :: Spec
spec = describe "execCommand: driving a single bike on a 3x3 grid" $ do
        it "turn right and crash into own trail" $ do
             execCommand grid1 (MoveCommand 1 (Steer E)) `shouldBe` Left (Suicide 1)
        it "do nothing (go straight) and crash into wall" $ do
             execCommand grid1 (MoveCommand 1 (Straight)) `shouldBe` Left (Suicide 1)
        it "turn left and survive" $ do
             execCommand grid1 (MoveCommand 1 (Steer W)) `shouldBe` (Right (Bike 1 W (0,2) [(1,2),(1,1), (2,1), (2,2)]))
