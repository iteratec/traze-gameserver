module Client where

import GameTypes
import Output

import Data.Aeson
import Data.UUID
import Data.List.Split

import System.Exit
import System.IO
import System.Random

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TMVar

import Control.Monad
import Control.Monad.STM
import Data.ByteString.Lazy (toStrict, fromStrict)

import qualified Network.Mosquitto as M
import Network.Mosquitto.Internal.Types
import qualified Data.ByteString as BS

import Data.String.Conversions

import Options

data ClientOptions = ClientOptions {
    optPlayerName :: String,
    optBrokerHost :: String,
    optBrokerPort :: Int
} deriving (Show, Eq)

instance Options.Options ClientOptions where
    defineOptions = pure ClientOptions
        <*> simpleOption "nick" "guest"
            "Your public ingame nick"
        <*> simpleOption "broker" "sandbox.hh.iteratec.de"
            "The MQTT broker hostname hosting the traze game"
        <*> simpleOption "port" 1883
            "The port to connect to the MQTT broker on" 

data ClientMessageType = JoinAcceptance | TickerMessage 

main :: IO ()
main = runCommand $ \opts _ -> do
    let playerNick = optPlayerName opts

    localInputQueue <- atomically $ newTQueue
    mqttQueue <- atomically $ newTQueue

    sessionVar <- atomically $ newEmptyTMVar

    _ <- forkIO $ clientMqttThread sessionVar mqttQueue opts playerNick

    _ <- forkIO $ forever $ inputToMessage sessionVar localInputQueue mqttQueue

    inputProcess <- async $ forever $ inputThread localInputQueue

    _ <- wait inputProcess
    return ()

inputToMessage :: TMVar (Int, String) -> TQueue Command -> TQueue (String, BS.ByteString) -> IO ()
inputToMessage sessionVar commandQueue mqttQueue = do
   (pid, session) <- atomically $ readTMVar sessionVar
   command <- atomically $ readTQueue commandQueue
   putStrLn $ "sending command: " ++ (charFromCommand command) : []
   case (command) of
       (MoveCommand _ m) -> atomically $ writeTQueue mqttQueue ("traze/1/" ++ (show pid) ++ "/steer", toStrict $ encode (SteerInput (getCourse m) session))
       (Quit _) -> atomically $ writeTQueue mqttQueue ("traze/1/" ++ (show pid) ++ "/bail", toStrict $ encode (BailInput session))

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

charFromCommand :: Command -> Char
charFromCommand (MoveCommand _ (Steer N)) = 'N'
charFromCommand (MoveCommand _ (Steer W)) = 'W'
charFromCommand (MoveCommand _ (Steer S)) = 'S'
charFromCommand (MoveCommand _ (Steer E)) = 'E'
charFromCommand (MoveCommand _ Straight)  = ' '
charFromCommand (Quit _) = 'Q'

-- Reads a char from stdin and writes a command accordingly into the input queue
inputThread :: TQueue Command -> IO ()
inputThread queue = do
    char <- getInput
    case command char of
        Nothing -> return ()
        Just c -> do 
            atomically $ writeTQueue queue c

        where command :: Char -> Maybe Command
              command c = commandFromChar $ Just c

getInput :: IO Char
getInput = hSetEcho stdin False
    >> hSetBuffering stdin NoBuffering
    >> getChar

clientMqttThread :: TMVar (Int, String) -> TQueue (String, BS.ByteString) -> ClientOptions -> String -> IO ()
clientMqttThread sessionVar gridQueue opts nick = M.withMosquittoLibrary $ do
    mqttClientName <- randomIO 
    m <- M.newMosquitto True (toString mqttClientName) (Just ())
    M.setTls m "" $ Just ("", "")
    M.setTlsInsecure m True
    _ <- M.setReconnectDelay m True 2 30
    -- M.onLog m $ const putStrLn
    M.onConnect m $ \_ -> do
        putStrLn $ "connected to broker as " ++ (toString mqttClientName) ++ "\0"
        M.subscribe m 0 ("traze/1/player/" ++ (toString mqttClientName) ++ "\0")
        M.subscribe m 0 ("traze/1/ticker\0")
        M.publish m False 0 "traze/1/join\0" $ toStrict $ encode $ JoinInput nick (toString mqttClientName)
        putStrLn $ "sent join request as " ++ nick ++ ". waiting for server response"

    M.onMessage m (handleMessage sessionVar)

    M.onDisconnect m print
    --M.onSubscribe m $ curry print
    _ <- M.connect m (optBrokerHost opts) (optBrokerPort opts) 1200

    _ <- forkIO $ forever $ do
        (top, message) <- atomically $ readTQueue gridQueue
        M.publish m False 0 top message

    M.loopForever m
    M.destroyMosquitto m
    return ()

handleMessage :: TMVar (Int, String) -> Message -> IO ()
handleMessage mvar (Message _ top payl _ _) = do
    case (parseTopic top) of
        Just (JoinAcceptance) -> do 
            case decode $ fromStrict payl of
                Just (AcceptJoinRequestOutput pid _ session _ pos) -> do
                    putStrLn ("spawned with pid " ++ (show pid) ++ " at position " ++ (show pos))
                    putStrLn "steer your bike with w, a, s, d. Press q to bail."
                    atomically $ putTMVar mvar (pid, session)
                Nothing -> return ()
        Just (TickerMessage) -> do
            case decode $ fromStrict payl of
                Just (DeathTick _ fragger casulty) -> do
                    (pid, _) <- atomically $ readTMVar mvar
                    when (casulty == pid) $ do
                        putStrLn("you died.")
                        exitWith ExitSuccess
                    when (fragger == pid) (putStrLn "you fragged. Keep doing that.")
                Nothing -> return ()
        Nothing -> putStrLn ("received unexpected message with topic: " ++ top ++ " and payload: " ++ (cs payl))

 
parseTopic :: String -> Maybe ClientMessageType
parseTopic top = case (splitOn "/" top) of
     ("traze" : _ : "player" : _ : []) -> Just $ JoinAcceptance
     ("traze" : _ : "ticker"  : []) -> Just $ TickerMessage
     _ -> Nothing
