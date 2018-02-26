module Main where

import GameTypes
import GameLogic
import SpawnPlayer
import SpawnQueue
import Mqtt
import Config

import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM
import Control.Monad.Loops
import Debug.Trace

main :: IO ()
main = clearScreen
    >> initialGrid
    >>= runGrid

oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

runGrid :: Grid -> IO ()
runGrid grid = do

    config <- getConfig

    -- outgoing messages via mqtt
    mqttQueue <- atomically $ newTQueue

    -- incomming commands
    inputQueue <- atomically $ newTQueue

    -- new game states
    gridQueue <- atomically $ newTQueue

    _ <- forkIO $ mqttThread mqttQueue inputQueue config
    _ <- forkIO $ forever $ atomically $ castGridThread gridQueue mqttQueue

    gameProcess <- async $ gameThread grid gridQueue inputQueue

    wait gameProcess
    return ()

initialGrid :: IO Grid
initialGrid = return $ Grid (64,64) [] []

gameThread :: Grid -> TQueue Grid -> TQueue Command -> IO ()
gameThread grid gq cq = do
    _ <- iterateUntilM (\_->False) (gameSTM gq cq) grid
    return ()

gameSTM :: TQueue Grid -> TQueue Command -> Grid -> IO (Grid)
gameSTM gridQueue commandQueue grid = do
    threadDelay (trace "tick" sampleLength)
    commands <- atomically $ liftM removeDuplicateCommands $ flushTQueue commandQueue
    let (grid', _) = play grid commands
    let grid'' = respawnPlayerIfNeeded grid'
    atomically $ writeTQueue gridQueue (trace (show grid'') grid'')
    return grid''

respawnPlayerIfNeeded :: Grid -> Grid
respawnPlayerIfNeeded grid@(Grid _ bs queue)
    | (length (bs ++ (map unQueueItem queue))) < 2  = fst $ spawnPlayer grid 
    | otherwise = grid

