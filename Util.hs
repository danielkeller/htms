module Util (
    Header,
    msgType, flags, seqNum,
    Message,
    header, body,
    MsgHandler,
    mainLoop
) where

import System.Environment
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
import Control.Monad.Trans.State.Strict

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

--parameters are origin address, message, and a message box for a state modifier function
type MsgHandler s = SockAddr -> Message -> MVar (s -> IO s) -> IO ()

--recvAllFrom = sequence . repeat $ recvFrom sock 1024
--or performAll = sequence . repeat

mainLoop :: s -> Map.Map Int8 (MsgHandler s) -> IO ()
mainLoop initState actions = withSocketsDo $ do

    --create the socket
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet torquePort iNADDR_ANY)

    --start tracking state
    stateChanger <- newEmptyMVar
    forkIO $ trackState stateChanger initState

    --start recieving data
    process stateChanger sock

    where
        trackState mvar state = do
            changer <- takeMVar mvar
            changer state >>= trackState mvar

        process mvar sock = do
            (bytes, sender) <- recvFrom sock 1024
            let msg = toMessage bytes
            case Map.lookup (msgType $ header msg) actions of

                Just handler -> do
                        handler sender msg mvar
                        process mvar sock
                        return () --not reached

                Nothing      -> do
                        putStrLn $ "got unknown message #" ++ show (msgType $ header msg)
                        process mvar sock
