-- Echo server program
module Main where

import qualified Data.ByteString.Char8 as B
import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.Network
import qualified Data.Streaming.Network as DSN
import Data.Streaming.Network.Internal (HostPreference(..))
import qualified Control.Concurrent.MVar as IM
import Control.Monad.IO.Class (liftIO)

forkFiberAndWait :: Fiber a -> IO ()
forkFiberAndWait f = do
    m <- IM.newEmptyMVar 
    forkFiber (f >> (liftIO $ IM.putMVar m ()))
    IM.takeMVar m
    return ()

main = forkFiberAndWait main'

main' :: Fiber ()
main' = withSocketsDo $ do
    sock <- liftIO $ DSN.bindPortTCP 3000 HostIPv4Only
    mainLoop sock

mainLoop :: Socket -> Fiber ()
mainLoop sock = do
    conn <- accept sock
    forkFiber $ runConn conn            -- run our server's logic
    mainLoop sock           -- repeat
 
runConn :: (Socket, SockAddr) -> Fiber ()
runConn (sock, _) = do
    m <- recv sock 100
    send sock $ B.pack "Hello!\n"
    send sock m
    close sock
