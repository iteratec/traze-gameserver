module RemoveDuplicateCommandSpec (spec) where

import GameTypes
import GameLogic

import Test.Hspec

spec :: Spec
spec =
    describe "removeDuplicateCommands" $ do
        it "has duplicates" $ do
            removeDuplicateCommands [MoveCommand 1 (Steer N), MoveCommand 2 (Steer E), Quit 1] `shouldBe` [Quit 1, MoveCommand 2 (Steer E)] 
            removeDuplicateCommands [MoveCommand 1 (Steer N), Quit 3, MoveCommand 2 (Steer E), Quit 1, Quit 3] `shouldBe` [Quit 1, MoveCommand 2 (Steer E), Quit 3] 
        it "has no duplicates" $ do
            removeDuplicateCommands [MoveCommand 1 (Steer N)] `shouldBe` [MoveCommand 1 (Steer N)]

