module Main where

import GameTypes
import GameLogic
import SpawnPlayer
import SpawnQueue
import Mqtt
import Output
import Instance
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

    -- new ticker notifications
    tickerQueue <- atomically $ newTQueue

    _ <- forkIO $ mqttThread mqttQueue inputQueue newPlayerQueue config
    _ <- forkIO $ forever $ atomically $ castInstanceThread gameStateQueue mqttQueue
    _ <- forkIO $ forever $ atomically $ castNewPlayerThread newPlayerQueue mqttQueue
    _ <- forkIO $ forever $ atomically $ castTickThread tickerQueue mqttQueue
    _ <- forkIO $ forever $ do
        threadDelay $ 5 * oneSecond
        atomically $ castGameInstancesThread gameStateQueue mqttQueue

    gameProcess <- async $ gameThread inst gameStateQueue inputQueue newPlayerQueue tickerQueue

    wait gameProcess
    return ()

initialGrid :: IO Grid
initialGrid = return $ Grid (62,62) [] []

initialInstance :: IO Instance
initialInstance = do
    grid <- initialGrid
    return $ Instance grid "1" []

gameThread :: Instance -> TQueue Instance -> TQueue Interaction -> TQueue Player -> TQueue Tick -> IO ()
gameThread inst instq interq playerq tickerq = do
    _ <- iterateUntilM (\_->False) (executeInstanceStep instq interq playerq tickerq) inst
    return ()

executeInstanceStep :: TQueue Instance -> TQueue Interaction -> TQueue Player -> TQueue Tick -> Instance -> IO (Instance)
executeInstanceStep output interactions playerQueue tickerq inst = do
    threadDelay sampleLength
    is <- atomically $ flushTQueue interactions -- TODO: remove duplicates
    (inst', deaths, newPlayers) <- runInstance inst is
    sendDeaths deaths tickerq
    mapM (\p -> atomically $ writeTQueue playerQueue p) newPlayers
    atomically $ writeTQueue output (trace (show inst') inst')
    return inst'

respawnPlayerIfNeeded :: Grid -> Grid
respawnPlayerIfNeeded grid@(Grid _ bs queue)
    | (length (bs ++ (map unQueueItem queue))) < 2  = fst $ spawnPlayer grid
    | otherwise = grid

sendDeaths :: [Death] -> TQueue Tick-> IO ()
sendDeaths deaths tickQueue = mapM_ (sendDeath tickQueue) deaths

sendDeath :: TQueue Tick -> Death -> IO ()
sendDeath tickQueue (Frag p1 p2) = atomically $ writeTQueue tickQueue (DeathTick "frag" p1 p2)
sendDeath tickQueue (Collision p1 p2) = atomically $ writeTQueue tickQueue (DeathTick "collision" p1 p2)
sendDeath tickQueue (Suicide p1) = atomically $ writeTQueue tickQueue (DeathTick "suicide" p1 p1)
