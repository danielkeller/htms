import qualified Data.Map as Map
import Data.DateTime
import Network.Socket
import Util
import Control.Concurrent
import Control.Applicative
import Control.Monad.Trans.State.Strict

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
actHeartbeat sender msg mvar = do
    --return $ putStrLn $ show num
    --newGame <- Game getCurrentTime
    --Map.insert sender newGame games
    print $ body msg
    putMVar mvar printinc
    return ()
    where
        printinc n = do
            print n
            return (n + 1)

main = mainLoop 0 $ Map.fromList
    [(32, actHeartbeat)]

