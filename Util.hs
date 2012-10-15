module Util (
    Header,
    msgType, flags, seqNum,
    Message,
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

type MsgHandler = SockAddr -> Message -> IO ()

mainLoop :: Map.Map Int8 MsgHandler -> IO ()
mainLoop actions = withSocketsDo $ do
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet torquePort iNADDR_ANY)
    process sock
    where
        process sock = do
            (bytes, sender) <- recvFrom sock 1024
            let msg = toMessage bytes
            case Map.lookup (msgType $ header msg) actions of
                Just handler -> (forkIO $ handler sender msg) >> return ()
                Nothing      -> putStrLn $ "got unknown message #" ++ show (msgType $ header msg)
            process sock
