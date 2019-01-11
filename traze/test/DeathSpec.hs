module DeathSpec (spec) where

import Traze.Internal.GameTypes
import Traze.Internal.InstanceTypes
import Traze.Internal.Instance

import TestData

import Test.Hspec

spec :: Spec
spec = describe "applyDeath" $ do
  it "colision of two players" $ do
    let players = [player1, player2]
        death   = Collision 1 2
    applyDeath death players `shouldBe` []

  it "player 1 frags player 2" $ do
    let players = [player1, player2]
        death   = Frag 1 2
    applyDeath death players `shouldBe` [incrementPlayerFrag player1]

  it "player 2 frags player 1" $ do
    let players = [player1, player2, player4, player3]
        death   = Frag 2 1
    applyDeath death players `shouldBe` [incrementPlayerFrag player2, player3, player4]

  it "player 1 kills himself" $ do
    let players = [player1, player2]
        death   = Suicide 1
    applyDeath death players `shouldBe` [player2]

  it "player 2 kills himself" $ do
    let players = [player1, player2]
        death   = Suicide 2
    applyDeath death players `shouldBe` [player1]

