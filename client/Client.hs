module Client where

import GameTypes
import Output
import Config

import Data.Aeson (encode, decode)

import System.IO

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TMVar

import Control.Monad
import Control.Monad.STM
import Debug.Trace
import Data.ByteString.Lazy (toStrict, fromStrict)

import qualified Network.Mosquitto as M
import Network.Mosquitto.Internal.Types
import qualified Data.ByteString as BS

import Data.String.Conversions

import Options

data ClientOptions = ClientOptions {
    optPlayerName :: String
} deriving (Show, Eq)

instance Options ClientOptions where
    defineOptions = pure ClientOptions
        <*> simpleOption "Nick" "guest"
            "Your public ingame nick"

main :: IO ()
main = runCommand $ \opts _ -> do
    config <- getConfig

    let playerNick = optPlayerName opts

    localInputQueue <- atomically $ newTQueue
    mqttQueue <- atomically $ newTQueue

    sessionVar <- atomically $ newEmptyTMVar

    _ <- forkIO $ clientMqttThread sessionVar mqttQueue config playerNick

    _ <- forkIO $ forever $ atomically $ inputToMessage sessionVar localInputQueue mqttQueue

    inputProcess <- async $ forever $ inputThread localInputQueue

    _ <- wait inputProcess
    return ()

inputToMessage :: TMVar (Int, String) -> TQueue Command -> TQueue (String, BS.ByteString) -> STM ()
inputToMessage sessionVar commandQueue mqttQueue = do
   (pid, session) <- readTMVar sessionVar
   command <- readTQueue commandQueue
   case (command) of
       (MoveCommand _ m) -> writeTQueue mqttQueue ("traze/1/" ++ (show pid) ++ "/steer", toStrict $ encode (SteerInput (getCourse m) session))
       (Quit _) -> writeTQueue mqttQueue ("traze/1/" ++ (show pid) ++ "/bail", toStrict $ encode (BailInput session))

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
    | c == 'q' = Just $ Quit 1
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

clientMqttThread :: TMVar (Int, String) -> TQueue (String, BS.ByteString) -> Config -> String -> IO ()
clientMqttThread sessionVar gridQueue config nick = M.withMosquittoLibrary $ do
    m <- M.newMosquitto True nick (Just ())
    M.setTls m "" "" ""
    M.setTlsInsecure m True
    _ <- M.setReconnectDelay m True 2 30
    M.onLog m $ const putStrLn
    M.onConnect m $ \c -> do
        putStrLn "connected to broker"
        print c
        M.subscribe m 0 ("traze/1/player/" ++ nick)
        M.publish m False 0 "traze/1/join" $ toStrict $ encode $ JoinInput nick

    M.onMessage m (handleMessage sessionVar)

    M.onDisconnect m print
    M.onSubscribe m $ curry print
    _ <- M.connect m (brokerHost config) (brokerPort config) 1200

    _ <- forkIO $ forever $ do
        (top, message) <- atomically $ readTQueue gridQueue
        M.publish m False 0 top message

    M.loopForever m
    M.destroyMosquitto m
    return ()

handleMessage :: TMVar (Int, String) -> Message -> IO ()
handleMessage mvar (Message _ top payl _ _) = do
    case decode $ fromStrict payl of
        Just (AcceptJoinRequestOutput pid _ session _ pos) -> do
            putStrLn ("spawned with pid " ++ (show pid) ++ " at position " ++ (show pos))
            atomically $ putTMVar mvar (pid, session)
        Nothing ->
            putStrLn ("received unexpected message with topic: " ++ top ++ " and payload: " ++ (cs payl))
