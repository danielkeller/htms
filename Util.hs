module Util (
    Header,
    msgType, flags, seqNum,
    Message,
    header, body,
    MsgHandler,
    mainLoop
) where

import Network hiding (accept, sendTo, recvFrom)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B.Lazy
import Control.Concurrent
import Control.Monad
import qualified Data.Map as Map
import Data.Int
import qualified Data.Binary as Bin

torquePort = 28002

--torque data structures

-- message id, flags, seq number
data Header = Header { msgType :: Int8, flags :: Int8, seqNum :: Int32 }

takeHeader msg = Header
     (Bin.decode $ B.Lazy.fromChunks [B.take 1 msg] :: Int8)
     (Bin.decode $ B.Lazy.fromChunks [B.take 1 $ B.drop 1 msg] :: Int8)
     (Bin.decode $ B.Lazy.fromChunks [B.take 4 $ B.drop 1 msg] :: Int32)

takeMessage msg = B.drop 6 msg

data Message = Message { header :: Header, body :: B.ByteString }

toMessage msg = Message (takeHeader msg) (takeMessage msg)

--control types

type StateMod s = (s -> IO s) -> IO ()

--parameters are origin address, message, and a function to safely modify global state
type MsgHandler s = SockAddr -> Message -> StateMod s -> IO ()

--lazily read everything from a socket
recvAllFrom sock = repeat $ recvFrom sock 1024

--convenience function
mapBind from to = map (>>= to) from

mainLoop :: s -> Map.Map Int8 (MsgHandler s) -> IO ()
mainLoop initState actions = withSocketsDo $ do

    --create the socket
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet torquePort iNADDR_ANY)

    --setup state tracking
    initMVar <- newMVar initState
    let modState = modifyMVar_ initMVar -- :: StateMod

    let process (bytes, sender) = do
        let msg = toMessage bytes
        case Map.lookup (msgType $ header msg) actions of
            Just handler -> void $ forkIO $ handler sender msg modState
            Nothing      -> putStrLn $ "got unknown message #" ++ show (msgType $ header msg)

    --start recieving data
    sequence_ $ recvAllFrom sock `mapBind` process
