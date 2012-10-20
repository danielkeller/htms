module Main (
    main
) where

import qualified Data.Map as Map
import Data.DateTime
import Network.Socket
import Control.Concurrent
import Control.Monad

import Util

torquePort = 28002

--message types
t_listreq = 6
t_listresp = 8
t_inforeq = 10
t_inforesp = 12
t_heartbeat = 22

data Game = Game { lastSeen :: DateTime }
    deriving (Eq, Ord, Show)
type Games = Map.Map SockAddr Game

actHeartbeat :: MsgHandler Games
actHeartbeat sender msg modState = do
    modState $ \ games -> do
        time <- getCurrentTime
        return $ Map.insert sender (Game time) games

main = mainLoop Map.empty $ Map.fromList
    [(32, actHeartbeat)]

