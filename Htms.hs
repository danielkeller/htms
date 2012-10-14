import System.Environment
import Network hiding (accept)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.ByteString.Char8 as B
import Control.Concurrent
import Control.Monad

torquePort = 28002

main = withSocketsDo $ do
    addr <- getAddrInfo torquePort
    sock <- socket AF_INET Datagram defaultProtocol
    bindSocket sock (addrAddress addr)
    process sock
    where process sock = do
        (msg, _, sender) <- recvFrom sock 1024
        putStrLn msg
        process sock

{- 
main = withSocketsDo $
    (listenOn $ PortNumber 6669) >>= accLoop

accLoop sock = do
    (conn, _) <- accept sock
    forkIO $ reply conn
    accLoop sock

reply conn = do
    str <- recv conn 4096
    print str
    if not $ B.null str
        then reply conn
        else return ()
-}
