import Test.Hspec
import HackaTron

bike1 :: Bike
bike1 = Bike 1 E (2,1) [ (1,1) , (0,1) ]

bike2 :: Bike
bike2 = Bike 2 N (2,7) [ (2,6) , (2,5) , (2,4) ]

grid1 :: Grid
grid1 = Grid (3,3) [(Bike 1 N (1,2) [(1,1), (2,1), (2,2)])] [] 

grid2 :: Grid
grid2 = Grid (4,4) [
    (Bike 1 E (1,3) [(0,3), (0,2), (0,1)]),
    (Bike 2 W (1,2) [(2,2), (3,2), (3,3)])
    ] []

grid3 :: Grid
grid3 = Grid (3,3) [
    (Bike 1 W (0,2) []),
    (Bike 2 E (3,2) [])
    ] []

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

-- XO1
-- X11
-- XXX
--
    describe "execCommand: driving a single bike on a 3x3 grid" $ do
        it "turn right and crash into own trail" $ do
             execCommand grid1 (MoveCommand 1 (Steer TurnRight)) `shouldBe` Left (Suicide 1)
        it "do nothing (go straight) and crash into wall" $ do
             execCommand grid1 (MoveCommand 1 (Straight)) `shouldBe` Left (Suicide 1)
        it "turn left and survive" $ do
             execCommand grid1 (MoveCommand 1 (Steer TurnLeft)) `shouldBe` (Right (Bike 1 W (0,2) [(1,2),(1,1), (2,1), (2,2)]))

-- 1OX2
-- 1O22
-- 1XXX
-- XXXX

    describe "play: two bikes on a 4x4 grid" $ do
        it "both dont react (straight): 2 crashes in 1" $ do
             play grid2 [] `shouldBe` (Grid (4,4) [(Bike 1 E (2,3) [(1,3), (0,3), (0,2), (0,1)])] [], [Frag 1 2])
        it "1 steers left: 1 kisses the wall, two crashes in 1" $ do
              play grid2 [(MoveCommand 1 (Steer TurnLeft))] `shouldBe` (Grid (4,4) [] [], [Suicide 1, Frag 1 2])
        it "1 steers right: each runs into the other" $ do
             play grid2 [(MoveCommand 1 (Steer TurnRight))] `shouldBe` (Grid (4,4) [] [], [Frag 2 1, Frag 1 2])
        it "1 steers right, 2 steers left: 2 is fine but 1 crashes in them" $ do
             play grid2 [(MoveCommand 1 (Steer TurnRight)),(MoveCommand 2 (Steer TurnLeft))] `shouldBe` (Grid (4,4) [Bike 2 S (1,1) [(1,2),(2,2), (3,2), (3,3)]] [], [Frag 2 1])

-- XXX
-- 1X2
-- XXX

    describe "play: two bikes on a 3x3 grid" $ do
        it "both go straight, reaching the same sqare at the same time" $ do
            play grid3 [] `shouldBe` (Grid (3,3) [] [], [Suicide 1, Suicide 2])


    describe "spawn player" $ do
        it "spawn second player on grid" $ do
            let (grid', Just newBike) = (spawnPlayer grid1)
            length (unBikes grid') `shouldBe` 1
            length (unQueue grid') `shouldBe` 1
            ((unCurrentLocation newBike) `elem` allBikeTrailCords (unBikes grid')) `shouldBe` False
           
