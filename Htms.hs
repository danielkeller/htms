module Main (
    main
) where

import qualified Data.Map as Map
import Data.DateTime
import Network.Socket
import Util
import Control.Concurrent
import Control.Applicative
import Control.Monad

torquePort = 28002

--message types
t_listreq = 6
t_listresp = 8
t_inforeq = 10
t_inforesp = 12
t_heartbeat = 22

data Game = Game { lastSeen :: DateTime }

type Games = Map.Map SockAddr Game

actHeartbeat :: MsgHandler Integer
actHeartbeat sender msg modState = do
    modState $ \s -> do
        print s
        threadDelay 1000000
        print (s + 1)
        print ""
        return (s + 1)
    threadDelay 1000000
    modState $ \s -> do
        print s
        threadDelay 1000000
        print (s - 1)
        print ""
        return (s - 1)

main = mainLoop 0 $ Map.fromList
    [(32, actHeartbeat)]

