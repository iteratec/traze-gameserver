module Client where

import GameTypes
import GameLogic
import Output
import SpawnPlayer
import SpawnQueue
import Mqtt
import Config

import Data.Aeson

import System.IO
import System.Console.ANSI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM
import Control.Monad.Loops
import Debug.Trace
import Data.ByteString.Lazy (toStrict, fromStrict)

import qualified Network.Mosquitto as M
import Network.Mosquitto.Internal.Types

import qualified Data.ByteString as BS

main :: IO ()
main = do
    config <- getConfig

    localInputQueue <- atomically $ newTQueue
    mqttQueue <- atomically $ newTQueue

    _ <- forkIO $ clientMqttThread mqttQueue config

    _ <- forkIO $ forever $ atomically $ inputToMessage localInputQueue mqttQueue

    inputProcess <- async $ forever $ inputThread localInputQueue

    wait inputProcess
    return ()

inputToMessage :: TQueue Command -> TQueue (String, BS.ByteString) -> STM ()
inputToMessage commandQueue mqttQueue = do
   command <- readTQueue commandQueue
   case (command) of
       (MoveCommand pid m) -> writeTQueue mqttQueue ("traze/1/" ++ (show pid) ++ "/steer", toStrict $ encode (SteerInput (getCourse m) "bla"))
       _ -> return ()

getCourse :: Move -> Course
getCourse (Steer c) = c
getCourse _ = N

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
        Just c -> atomically $ trace (show c) $ writeTQueue queue c

        where command :: Char -> Maybe Command
              command c = commandFromChar $ Just c

getInput :: IO Char
getInput = hSetEcho stdin False
    >> hSetBuffering stdin NoBuffering
    >> getChar

clientMqttThread :: TQueue (String, BS.ByteString) -> Config -> IO ()
clientMqttThread gridQueue config = M.withMosquittoLibrary $ do
    m <- M.newMosquitto True (clientName config) (Just ())
    M.setTls m "" "" ""
    M.setTlsInsecure m True
    _ <- M.setReconnectDelay m True 2 30
    M.onLog m $ const putStrLn
    M.onConnect m $ \c -> do
        putStrLn "connected to broker"
        print c

    M.onDisconnect m print
    M.onSubscribe m $ curry print
    _ <- M.connect m (brokerHost config) (brokerPort config) 1200

    _ <- forkIO $ forever $ do
        (top, message) <- atomically $ readTQueue gridQueue
        M.publish m False 0 top message

    M.loopForever m
    M.destroyMosquitto m
    return ()

