import Test.Hspec
import Test.HUnit
import HackaTron

bike1 :: Bike
bike1 = Bike 1 E (2,1) [ (1,1) , (0,1) ]

bike2 :: Bike
bike2 = Bike 2 N (2,7) [ (2,6) , (2,5) , (2,4) ]

bike3 :: Bike
bike3 = Bike 3 W (1,1) []

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
             driveBike bike1 (Steer N) `shouldBe` ( Bike 1 N (2,2) [ (2,1), (1,1) , (0,1) ] )

        it "drive bike1 straight" $ do
             driveBike bike1 Straight `shouldBe` ( Bike 1 E (3,1) [ (2,1), (1,1) , (0,1) ] )

        it "turn bike1 to the south" $ do
             driveBike bike1 (Steer S) `shouldBe` ( Bike 1 S (2,0) [ (2,1), (1,1) , (0,1) ] )

        it "drive bike2 straight" $ do
             driveBike bike2 Straight `shouldBe` ( Bike 2 N (2,8) [ (2,7), (2,6) , (2,5) , (2,4) ] )

        it "turn bike2 to the west" $ do
             driveBike bike2 (Steer W) `shouldBe` ( Bike 2 W (1,7) [ (2,7), (2,6) , (2,5) , (2,4) ] )

        it "turn bike2 to the east" $ do
             driveBike bike2 (Steer E) `shouldBe` ( Bike 2 E (3,7) [ (2,7), (2,6) , (2,5) , (2,4) ] )

-- XO1
-- X11
-- XXX
--
    describe "execCommand: driving a single bike on a 3x3 grid" $ do
        it "turn right and crash into own trail" $ do
             execCommand grid1 (MoveCommand 1 (Steer E)) `shouldBe` Left (Suicide 1)
        it "do nothing (go straight) and crash into wall" $ do
             execCommand grid1 (MoveCommand 1 (Straight)) `shouldBe` Left (Suicide 1)
        it "turn left and survive" $ do
             execCommand grid1 (MoveCommand 1 (Steer W)) `shouldBe` (Right (Bike 1 W (0,2) [(1,2),(1,1), (2,1), (2,2)]))

-- 1OX2
-- 1O22
-- 1XXX
-- XXXX

    describe "play: two bikes on a 4x4 grid" $ do
        it "both dont react (straight): 2 crashes in 1" $ do
             play grid2 [] `shouldBe` (Grid (4,4) [(Bike 1 E (2,3) [(1,3), (0,3), (0,2), (0,1)])] [], [Frag 1 2])
        it "1 steers left: 1 kisses the wall, two crashes in 1" $ do
              play grid2 [(MoveCommand 1 (Steer N))] `shouldBe` (Grid (4,4) [] [], [Suicide 1, Frag 1 2])
        it "1 steers right: each runs into the other" $ do
             play grid2 [(MoveCommand 1 (Steer S))] `shouldBe` (Grid (4,4) [] [], [Frag 2 1, Frag 1 2])
        it "1 steers right, 2 steers left: 2 is fine but 1 crashes in them" $ do
             play grid2 [(MoveCommand 1 (Steer S)),(MoveCommand 2 (Steer S))] `shouldBe` (Grid (4,4) [Bike 2 S (1,1) [(1,2),(2,2), (3,2), (3,3)]] [], [Frag 2 1])

-- XXX
-- 1X2
-- XXX

    describe "play: two bikes on a 3x3 grid" $ do
        it "both go straight, reaching the same sqare at the same time" $ do
            play grid3 [] `shouldBe` (Grid (3,3) [] [], [Suicide 1, Suicide 2])


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


    describe "popFromQueue" $ do
        it "pop one item" $ do
            let (queue, bikes) = popFromQueue [(QueueItem 1 bike1)] [(MoveCommand 1 Straight)]
            assertEqual "queue is empty after pop" 0 $ length queue
            assertEqual "bike has spawned after pop" 1 $ length bikes
        it "don't pop item" $ do
            let (queue, bikes) = popFromQueue [(QueueItem 1 bike1)] [(MoveCommand 2 Straight)]
            assertEqual "queue is not empty after pop" 1 $ length queue
            assertEqual "bike has not spawned after pop" 0 $ length bikes
        it "don't pop item" $ do
            let (queue, bikes) = popFromQueue [(QueueItem 1 bike1),(QueueItem 1 bike2), QueueItem 1 bike3] [(MoveCommand 2 Straight)]
            assertEqual "queue is not empty after pop" 2 $ length queue
            assertEqual "bike has not spawned after pop" 1 $ length bikes

