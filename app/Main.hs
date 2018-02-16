module Main where

import GameTypes
import GameLogic
import Output
import SpawnPlayer
import Mqtt
import Config

import System.IO
import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM
import Control.Monad.Loops

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

    mqttQueue <- atomically $ newTQueue
    _ <- forkIO $ mqttThread mqttQueue config

    inputQueue <- atomically $ newTQueue
    _ <- forkIO $ forever $ inputThread inputQueue

    gridQueue <- atomically $ newTQueue
    _ <- forkIO $ forever $ atomically $ castGridThread gridQueue mqttQueue

    gameProcess <- async $ gameThread grid gridQueue inputQueue
    wait gameProcess
    return ()

initialGrid :: IO Grid
initialGrid = return $ fst $ spawnPlayer $ fst $ spawnPlayer (Grid (30,20) [] [])

commandFromChar :: Maybe Char -> Maybe Command
commandFromChar Nothing = Nothing
commandFromChar (Just c)
    | c == 'w' = Just $ MoveCommand 1 (Steer N)
    | c == 'a' = Just $ MoveCommand 1 (Steer W)
    | c == 's' = Just $ MoveCommand 1 (Steer S)
    | c == 'd' = Just $ MoveCommand 1 (Steer E)
    | c == 'i' = Just $ MoveCommand 2 (Steer N)
    | c == 'j' = Just $ MoveCommand 2 (Steer W)
    | c == 'k' = Just $ MoveCommand 2 (Steer S)
    | c == 'l' = Just $ MoveCommand 2 (Steer E)
    | otherwise = Nothing

-- Reads a char from stdin and writes a command accordingly into the input queue
inputThread :: TQueue Command -> IO ()
inputThread queue = do
    char <- getInput
    case command char of
        Nothing -> return ()
        Just c -> atomically $ writeTQueue queue c

        where command :: Char -> Maybe Command
              command c = commandFromChar $ Just c

gameThread :: Grid -> TQueue Grid -> TQueue Command -> IO ()
gameThread grid gq cq = do
    _ <- iterateUntilM gameOver (gameSTM gq cq) grid
    return ()

gameSTM :: TQueue Grid -> TQueue Command -> Grid -> IO (Grid)
gameSTM gridQueue commandQueue grid = do
    threadDelay sampleLength
    commands <- atomically $ liftM removeDuplicateCommands $ flushTQueue commandQueue
    let (grid', _) = play grid commands
    atomically $ writeTQueue gridQueue grid'
    return grid'

displayThread :: TQueue Grid -> IO ()
displayThread gridQueue = forever $ do
    grid <- atomically $ readTQueue gridQueue
    displayGrid grid

getInput :: IO Char
getInput = hSetEcho stdin False
    >> hSetBuffering stdin NoBuffering
    >> getChar

gameOver :: Grid -> Bool
gameOver (Grid _ [] []) = True
gameOver _ = False

displayGrid :: Grid -> IO Grid
displayGrid grid = do
    setCursorPosition 0 0
    printGrid grid
    putStrLn $ show grid
    return grid

