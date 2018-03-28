module RenderGridSpec (spec) where

import GameTypes
import GameLogic

import TestData

import Test.Hspec

spec :: Spec
spec =
    describe "render grid" $ do
        it "render grid 1" $ do
            getTiles grid1 `shouldBe` [[0,0,0],[0,1,1],[0,1,1]]
        it "render grid 2" $ do
            getTiles grid2 `shouldBe` [[0,1,1,1],[0,0,2,1],[0,0,2,0],[0,0,2,2]]
        it "render grid 3" $ do
            getTiles grid3 `shouldBe` [[0,1,0],[0,0,0],[0,2,0]]
