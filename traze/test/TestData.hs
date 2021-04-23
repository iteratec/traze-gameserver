module TestData where

import Traze.Internal.GameTypes
import Traze.Internal.InstanceTypes
import Data.UUID (fromString)
import Data.Maybe

bike1 :: Bike
bike1 = Bike 1 E (2,1) [ (1,1) , (0,1) ]

bike2 :: Bike
bike2 = Bike 2 N (2,7) [ (2,6) , (2,5) , (2,4) ]

bike3 :: Bike
bike3 = Bike 3 W (1,1) []

-- XO1
-- X11
-- XXX
grid1 :: Grid
grid1 = Grid (3,3) [(Bike 1 N (1,2) [(1,1), (2,1), (2,2)])] [] (Round 1 0)

-- 1OX2
-- 1O22
-- 1XXX
-- XXXX
grid2 :: Grid
grid2 = Grid (4,4) [
    (Bike 1 E (1,3) [(0,3), (0,2), (0,1)]),
    (Bike 2 W (1,2) [(2,2), (3,2), (3,3)])
    ] [] (Round 2 0)

-- XXX
-- 1X2
-- XXX
grid3 :: Grid
grid3 = Grid (3,3) [
    (Bike 1 W (0,1) []),
    (Bike 2 E (2,1) [])
    ] [] (Round 3 0)

-- Round not yet started
-- XXX
-- 1X2
-- XXX
grid4 :: Grid
grid4 = Grid (3,3) [
    (Bike 1 W (0,1) []),
    (Bike 2 E (2,1) [])
    ] [] (Round 4 20)

player1 :: Player
player1 = Player 1 "Player one" 0 0 "#FFFFFF" (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c") "mqtt1" (1,2)

player2 :: Player
player2 = Player 2 "Player two" 0 0 "#000000" (fromJust $ fromString "d090ac2f-36e9-4b71-88b3-84699aea7cc5") "mqtt2" (2,2)

player3 :: Player
player3 = Player 3 "Player three" 0 0 "#FF0000" (fromJust $ fromString "804dd1e8-545b-4981-b543-d47c4b507cbb") "mqtt3" (2,1)

player4 :: Player
player4 = Player 4 "Player four" 0 0 "#00FF00" (fromJust $ fromString "98e6fbcb-0a0e-45a3-ab2b-4840adf1e52d") "mqtt4" (1,1)



instance1 :: Instance
instance1 = Instance grid1 "blablubb" [player1]
