module MqttStructureSpec (spec) where

import MqttStructure

import Test.Hspec

spec :: Spec
spec = 
    describe "parseTopic" $ do
        it "parse traze/1/3/steer" $ do
           parseTopic "traze/1/3/steer" `shouldBe` (Just $ Steering "1" 3)
        it "parse traze/bla/4/bail" $ do
           parseTopic "traze/bla/4/bail" `shouldBe` (Just $ Bail "bla" 4)
        it "parse traze/foo/bar/steer" $ do
           parseTopic "traze/foo/bar/steer" `shouldBe` Nothing
        it "parse traze/afk/join" $ do
           parseTopic "traze/afk/join" `shouldBe` (Just $ Join "afk")
        it "parse asdf" $ do
           parseTopic "asdf" `shouldBe` Nothing
        it "parse traze/bla/bli/blubb" $ do
           parseTopic "traze/bla/bli/blubb" `shouldBe` Nothing
        it "parse traze/bla/bla/1/steer" $ do
           parseTopic "traze/bla/bla/1/steer" `shouldBe` Nothing
