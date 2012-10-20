module Util (
    Header(),
    msgType, flags, seqNum,
    Message(),
    header, body,
    MsgHandler(),
    mainLoop,
    udpSend
) where

import Network hiding (accept, sendTo, recvFrom)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as B.Lazy
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Int
import qualified Data.Binary as Bin
import Data.Binary (get, put)

torquePort = 28002

--torque data structures

-- message id, flags, seq number
data Header = Header { msgType :: Int8, flags :: Int8, seqNum :: Int32 }

instance Bin.Binary Header where
    put (Header msgType flags seqNum) =
        put msgType >> put flags >> put seqNum
    get = liftM3 Header get get get
{-
takeHeader msg = Header
     (Bin.decode $ B.Lazy.fromChunks [B.take 1 msg] :: Int8)
     (Bin.decode $ B.Lazy.fromChunks [B.take 1 $ B.drop 1 msg] :: Int8)
     (Bin.decode $ B.Lazy.fromChunks [B.take 4 $ B.drop 1 msg] :: Int32)
-}

tolazy bs = B.Lazy.fromChunks [bs]

takeHeader msg = Bin.decode $ tolazy $ msg :: Header
takeMessage msg = B.drop 6 msg

data Message = Message { header :: Header, body :: B.ByteString }

toMessage msg
    | B.length msg >= 6 = Just $ Message (takeHeader msg) (takeMessage msg)
    | otherwise         = Nothing

--control types

type StateMod s = (s -> IO s) -> IO () -- use as 'modState $ \s -> do'
--parameters are origin address, message, and a function to safely modify global state
type MsgHandler s = SockAddr -> Message -> StateMod s -> IO ()

mainLoop :: s -> Map.Map Int8 (MsgHandler s) -> IO ()
mainLoop initState actions = withSocketsDo $ do

    --create the socket
    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet torquePort iNADDR_ANY)

    --setup state tracking
    initMVar <- newMVar initState
    let modState = modifyMVar_ initMVar -- :: StateMod

    liftIO (forever $ recvFrom sock 1024) >>= \ (bytes, sender) -> do
        msg <- return $ toMessage bytes
        case Map.lookup (msgType $ header msg) actions of
            Just handler -> void $ forkIO $ handler sender msg modState
            Nothing      -> putStrLn $ "got unknown message #" ++ show (msgType $ header msg)

udpSend addr msg = do
    sock <- socket AF_INET Datagram defaultProtocol
    sendTo sock msg addr
