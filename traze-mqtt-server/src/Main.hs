{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Traze
import MqttStructure
import Mqtt
import Config

import Control.Concurrent
import Control.Concurrent.STM.TQueue

import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Reader

import Debug.Trace

import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.System

main :: IO ()
main = do
    config <- getConfig
    appEnv <- initAppEnv config
    stdGen <- getStdGen -- mkStdGen 1 for testing
    runTrazeMonad appEnv initialInstance stdGen executeInstance

newtype TrazeMonad a = TM { unTM :: RandT StdGen (ReaderT AppEnv (StateT Instance IO)) a }
  deriving (Functor, Applicative, MonadReader AppEnv, Monad, MonadRandom, MonadState Instance, MonadIO)

runTrazeMonad :: AppEnv -> Instance -> StdGen -> TrazeMonad a -> IO a
runTrazeMonad appEnv inst stdGen (TM m) =
  evalStateT (runReaderT (evalRandT m stdGen) appEnv) inst

oneSecond :: Integer
oneSecond = (10 :: Integer) ^ (6 :: Integer)

sampleLength :: Integer
sampleLength = oneSecond `div` 4

data AppEnv = AppEnv {
    config :: Config, 
    mqttQueue :: TQueue MqttMessage,
    inputQueue :: TQueue Interaction,
    gameStateQueue :: TQueue Instance,
    newPlayerQueue :: TQueue Player,
    tickerQueue :: TQueue Tick
}

initAppEnv :: (MonadIO m) => Config -> m AppEnv
initAppEnv config = liftIO $ do
    
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

    _ <- forkIO $ mqttThread mqttQueue inputQueue config
    _ <- forkIO $ forever $ atomically $ castInstanceThread gameStateQueue mqttQueue
    _ <- forkIO $ forever $ atomically $ castNewPlayerThread newPlayerQueue mqttQueue
    _ <- forkIO $ forever $ atomically $ castTickThread tickerQueue mqttQueue
    _ <- forkIO $ forever $ do
        threadDelay $ fromInteger $ 5 * oneSecond
        atomically $ castGameInstancesThread gameStateQueue mqttQueue
    return AppEnv {..}


executeInstance :: (MonadReader AppEnv m, MonadRandom m, MonadState Instance m, MonadIO m) => m ()
executeInstance = forever executeInstanceStep

initialGrid :: Grid
initialGrid = Grid (62,62) [] []

initialInstance :: Instance
initialInstance = Instance initialGrid "1" []

executeInstanceStep :: (MonadReader AppEnv m, MonadRandom m, MonadState Instance m, MonadIO m) => m ()
executeInstanceStep = do
    entryTime <- liftIO $ getSystemTime
    AppEnv {..} <- ask
    is <- liftIO $ atomically $ fmap nub $ flushTQueue inputQueue

    deaths <- stepInstance $ catMaybes $ map isGridCommand is
    gridStepTime <- liftIO $ getSystemTime
    let gridStepDuration = diffUTCTime (systemToUTCTime gridStepTime) (systemToUTCTime entryTime)
    liftIO $ putStrLn $ "gridStep took " ++ (show gridStepDuration)

    let joinRequests = catMaybes $ map isJoinRequest is

    newPlayers <- runSpawning $ maybeToList $ listToMaybe $ joinRequests

    liftIO $ atomically $ mapM_ (writeTQueue inputQueue) $ fmap JoinInteraction $ safeTail joinRequests

    spawnTime <- liftIO $ getSystemTime
    let spawnDuration = diffUTCTime (systemToUTCTime spawnTime) (systemToUTCTime gridStepTime)
    liftIO $ putStrLn $ "playerSpawn took " ++ (show spawnDuration)

    let computationTime = diffUTCTime (systemToUTCTime spawnTime) (systemToUTCTime entryTime)
    let remainingWindow = (fromInteger sampleLength) - (computationTime * realToFrac oneSecond)

    liftIO $ sendDeaths deaths tickerQueue
    liftIO $ mapM_ (\p -> atomically $ writeTQueue newPlayerQueue p) newPlayers
    inst <- get
    liftIO $ atomically $ writeTQueue gameStateQueue (traceShowId inst)

    liftIO $ putStrLn $ "sleeping for " ++ (show remainingWindow)
    liftIO $ threadDelay $ round remainingWindow

safeTail :: [a] -> [a]
safeTail [] = []
safeTail [_] = []
safeTail as = tail as

sendDeaths :: [Death] -> TQueue Tick-> IO ()
sendDeaths deaths tickQueue = mapM_ (sendDeath tickQueue) deaths

sendDeath :: TQueue Tick -> Death -> IO ()
sendDeath tickQueue (Frag p1 p2) = atomically $ writeTQueue tickQueue (DeathTick "frag" p1 p2)
sendDeath tickQueue (Collision p1 p2) = atomically $ writeTQueue tickQueue (DeathTick "collision" p1 p2)
sendDeath tickQueue (Suicide p1) = atomically $ writeTQueue tickQueue (DeathTick "suicide" p1 p1)
