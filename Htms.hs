module Main (
    main
) where

import qualified Data.Map as Map
import Data.DateTime
import Data.Int
import Network.Socket
import Control.Concurrent
import Control.Monad
import Control.Applicative

import Util

logMsg msg = do
    time <- getCurrentTime
    putStrLn $ show time ++ ": " ++ msg

torquePort = 28002

--message types
t_listreq = 6
t_listresp = 8
t_inforeq = 10
t_inforesp = 12
t_heartbeat = 22

data GameInfo = GameInfo {
    game :: String, mission :: String,
    maxPlayers :: Int8, bots :: Int8, players :: [Int32],
    flags :: Int8, cpu :: Int16,
    region :: Int32, version :: Int32 } --TODO: different timer on info?
    deriving (Eq, Ord, Show)

noInfo = GameInfo "" "" 0 0 [] 0 0 0 0

data Game = Game { lastSeen :: DateTime, info :: GameInfo }
    deriving (Eq, Ord, Show)
type Games = Map.Map SockAddr Game

actHeartbeat :: MsgHandler Games
actHeartbeat sender msg modState = do
    logMsg $ "received heartbeat from " ++ show sender
    time <- getCurrentTime
    modState $ \ games -> do
        return $ Map.insert sender (Game time noInfo) games
    threadDelay $ 5 * {- 60 * -} 1000000 --wait 5 minutes
    modState $ \ games ->
        if (lastSeen <$> Map.lookup sender games) == Just time
            then do
                logMsg $ "server " ++ show sender ++ " timed out, dropping"
                return $ Map.delete sender games --timed out (not updated since we did)
            else return games --still good

reqInfo server = do
    sendTo

main = mainLoop Map.empty $ Map.fromList
    [(32, actHeartbeat)]

