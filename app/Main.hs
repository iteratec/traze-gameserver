module Main where

import GameTypes
import GameLogic
import SpawnPlayer
import SpawnQueue
import Mqtt
import Config
import Instance

import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM
import Control.Monad.Loops
import Debug.Trace

main :: IO ()
main = do
    executeInstance =<< initialInstance
    return ()

oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

executeInstance :: Instance -> IO ()
executeInstance inst = do

    config <- getConfig

    -- outgoing messages via mqtt
    mqttQueue <- atomically $ newTQueue

    -- incomming commands
    inputQueue <- atomically $ newTQueue

    -- new game states
    gameStateQueue <- atomically $ newTQueue

    -- new Players
    newPlayerQueue <- atomically $ newTQueue

    _ <- forkIO $ mqttThread mqttQueue inputQueue newPlayerQueue config
    _ <- forkIO $ forever $ atomically $ castInstanceThread gameStateQueue mqttQueue
    _ <- forkIO $ forever $ atomically $ castNewPlayerThread newPlayerQueue mqttQueue

    gameProcess <- async $ gameThread inst gameStateQueue inputQueue newPlayerQueue

    wait gameProcess
    return ()

initialGrid :: IO Grid
initialGrid = return $ Grid (62,62) [] []

initialInstance :: IO Instance
initialInstance = do
    grid <- initialGrid
    return $ Instance grid "1" []

gameThread :: Instance -> TQueue Instance -> TQueue Interaction -> TQueue Player -> IO ()
gameThread inst instq interq playerq= do
    _ <- iterateUntilM (\_->False) (executeInstanceStep instq interq playerq) inst
    return ()

executeInstanceStep :: TQueue Instance -> TQueue Interaction -> TQueue Player -> Instance -> IO (Instance)
executeInstanceStep output interactions playerQueue inst = do
    threadDelay sampleLength
    is <- atomically $ flushTQueue interactions -- TODO: remove duplicates
    (inst', deaths, newPlayers) <- runInstance inst is
    mapM (\p -> atomically $ writeTQueue playerQueue p) newPlayers
    atomically $ writeTQueue output (trace (show inst') inst')
    return inst'


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

