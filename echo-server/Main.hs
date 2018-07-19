-- Echo server program
module Main where

import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import Control.Concurrent (threadDelay)
import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.Network
import Control.Concurrent.Fiber.Network.Internal (fiber)
import qualified Control.Concurrent.MVar as IM

forkFiberAndWait :: Fiber a -> IO ()
forkFiberAndWait f = do
    m <- IM.newEmptyMVar 
    forkFiber (f >> (liftIO $ IM.putMVar m ()))
    IM.takeMVar m
    return ()
-- This mesteriously would throw `ClosedByInterruptException`
-- So maybe there are still some problems with concurrency
-- main = forkFiber main' >> loop
--   where loop = do
--           threadDelay 1000
--           loop
main = forkFiberAndWait main'

main' :: Fiber ()
main' = withSocketsDo $ do
    sock <- socket AF_INET (ServerSocket Stream) 5    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4242 0)   -- listen on TCP port 4242.
    listen sock 2                              -- set a max of 2 queued connections
    mainLoop sock


mainLoop :: Socket -> Fiber ()
mainLoop sock = do
    conn <- accept sock     -- accept a connection and handle it
    liftIO $ forkFiber $ runConn conn            -- run our server's logic
    mainLoop sock           -- repeat
 
runConn :: (Socket, SockAddr) -> Fiber ()
runConn (sock, _) = do
    m <- recv sock 100
    send sock $ B.pack "Hello!\n"
    send sock m
    close sock
