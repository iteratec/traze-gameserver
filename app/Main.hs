module Main where

import GameTypes
import Output
import SpawnPlayer
import Traze

import System.IO
import System.Timeout
import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Loops

import Data.Maybe

main :: IO ()
main = clearScreen
    >> initialGrid
    >>= (iterateUntilM gameOver step)
    >> clearScreen

oneSecond :: Int
oneSecond = (10 :: Int) ^ (6 :: Int)

sampleLength :: Int
sampleLength = oneSecond `div` 4

step :: Grid -> IO Grid
step grid = sample sampleLength getInput
    >>= \ inputMove ->
       displayGrid $ updateState grid (commandFromChar inputMove)

initialGrid :: IO Grid
initialGrid = return $ fst $ spawnPlayer (Grid (20,10) [] [])

updateState :: Grid -> Maybe Command -> Grid
updateState g c = fst $ play g (maybeToList c)

commandFromChar :: Maybe Char -> Maybe Command
commandFromChar Nothing = Nothing
commandFromChar (Just c)
    | c == 'w' = Just $ MoveCommand 1 (Steer N)
    | c == 'a' = Just $ MoveCommand 1 (Steer W)
    | c == 's' = Just $ MoveCommand 1 (Steer S)
    | c == 'd' = Just $ MoveCommand 1 (Steer E)
    | otherwise = Nothing
    

getInput :: IO Char
getInput = hSetEcho stdin False
    >> hSetBuffering stdin NoBuffering
    >> getChar

gameOver :: Grid -> Bool
gameOver (Grid _ [] []) = True
gameOver _ = False

displayGrid :: Grid -> IO Grid
displayGrid grid = setCursorPosition 0 0
    >> printGrid grid
    >> return grid

sample :: Int -> IO a -> IO (Maybe a)
sample n f
    | n < 0 = fmap Just f
    | n == 0 = return Nothing
    | otherwise =
        concurrently (timeout n f) (threadDelay n)
            >>= \ (result, _) -> return result
