import Test.Hspec
import HackaTron

bike1 :: Bike
bike1 = Bike 1 E (2,1) [ (1,1) , (0,1) ]

bike2 :: Bike
bike2 = Bike 2 N (2,7) [ (2,6) , (2,5) , (2,4) ]

main :: IO ()
main = hspec $ do
    describe "driveBike: driving a single bike" $ do
        it "turn bike1 to the north" $ do
             driveBike bike1 (Steer TurnLeft) `shouldBe` ( Bike 1 N (2,2) [ (2,1), (1,1) , (0,1) ] )

        it "drive bike1 straight" $ do
             driveBike bike1 Straight `shouldBe` ( Bike 1 E (3,1) [ (2,1), (1,1) , (0,1) ] )

        it "turn bike1 to the south" $ do
             driveBike bike1 (Steer TurnRight) `shouldBe` ( Bike 1 S (2,0) [ (2,1), (1,1) , (0,1) ] )

        it "drive bike2 straight" $ do
             driveBike bike2 Straight `shouldBe` ( Bike 2 N (2,8) [ (2,7), (2,6) , (2,5) , (2,4) ] )

        it "turn bike2 to the west" $ do
             driveBike bike2 (Steer TurnLeft) `shouldBe` ( Bike 2 W (1,7) [ (2,7), (2,6) , (2,5) , (2,4) ] )

        it "turn bike2 to the east" $ do
             driveBike bike2 (Steer TurnRight) `shouldBe` ( Bike 2 E (3,7) [ (2,7), (2,6) , (2,5) , (2,4) ] )


