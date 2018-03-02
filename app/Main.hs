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

    -- new ticker notifications
    tickerQueue <- atomically $ newTQueue

    _ <- forkIO $ mqttThread mqttQueue inputQueue config
    _ <- forkIO $ forever $ atomically $ castGridThread gridQueue mqttQueue

    gameProcess <- async $ gameThread grid gridQueue inputQueue tickerQueue

    wait gameProcess
    return ()

initialGrid :: IO Grid
initialGrid = return $ Grid (64,64) [] []

gameThread :: Grid -> TQueue Grid -> TQueue Command -> TQueue Tick -> IO ()
gameThread grid gq cq tq = do
    _ <- iterateUntilM (\_->False) (gameSTM gq cq tq) grid
    return ()

gameSTM :: TQueue Grid -> TQueue Command -> TQueue Tick -> Grid -> IO (Grid)
gameSTM gridQueue commandQueue tickQueue grid = do
    threadDelay (trace "tick" sampleLength)
    commands <- atomically $ liftM removeDuplicateCommands $ flushTQueue commandQueue
    let (grid', deaths) = play grid commands
    sendDeaths deaths tickQueue
    let grid'' = respawnPlayerIfNeeded grid'
    atomically $ writeTQueue gridQueue (trace (show grid'') grid'')
    return grid''

respawnPlayerIfNeeded :: Grid -> Grid
respawnPlayerIfNeeded grid@(Grid _ bs queue)
    | (length (bs ++ (map unQueueItem queue))) < 2  = fst $ spawnPlayer grid
    | otherwise = grid

sendDeaths :: [Death] -> TQueue -> IO ()
sendDeaths deaths  tickQueue = map (sendDeath tickQueue) deaths

sendDeath :: TQueue -> Death -> IO ()
sendDeath tickQueue (Frag p1 p2) = writeTQueue tickQueue (DeathTick "Player fragged" p1 p2)
sendDeath tickQueue (Collision p1 p2) = writeTQueue tickQueue (DeathTick "Two players collided" p1 p2)
sendDeath tickQueue (Suicide p1) = writeTQueue tickQueue (DeathTick "Player killed himself" p1 p1)
