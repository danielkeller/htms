import qualified Data.Map as Map
import Util

torquePort = 28002

actHeartbeat :: MsgHandler
actHeartbeat sender (header, msg) = do
    putStrLn $ "badump: " ++ show sender

main = mainLoop $ Map.fromList
    [(32, actHeartbeat)] --actually 22

