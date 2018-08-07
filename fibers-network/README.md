# fibers-network

Non-blocking network IO for eta-fibers. Contains most basic functions from `network` and essentially has the same API but uses `Fiber` monad.

Aside from functions from `network`, it also contains some more primitive functions for `Fiber` like `threadWait*`. Basic IO functions like `bindport*` and `readRawBufferPtr`/`writeRawBufferPtr` are also included. They also don't differ much from there original version except they're using `Fiber` monad and are non-blocking.

The `Socket` type is imported from `network`, so you can mix non-blocking functions here and blocking functions from `network` if you want. This can be convenient if you are using libraries that performs IO operations but does not support Fiber yet.

Here is an echo-server example:

```haskell
-- Echo server program
module Main where

import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.Network
import qualified Data.Streaming.Network as DSN
import Data.Streaming.Network.Internal (HostPreference(..))
import qualified Control.Concurrent.MVar as IM
import Control.Monad.IO.Class (liftIO)

-- have the mainthread wait on the fiber thread so program doesn't just terminate
forkFiberAndWait :: Fiber a -> IO ()
forkFiberAndWait f = do
    m <- IM.newEmptyMVar 
    forkFiber (f >> (liftIO $ IM.putMVar m ()))
    IM.takeMVar m
    return ()

-- mainthread
main = forkFiberAndWait main'

-- the "real mainthread"
main' :: Fiber ()
main' = withSocketsDo $ do
    sock <- liftIO $ DSN.bindPortTCP 3000 HostIPv4Only
    mainLoop sock

mainLoop :: Socket -> Fiber ()
mainLoop sock = do
    conn <- accept sock
    forkFiber $ runConn conn  -- run our server's logic
    mainLoop sock             -- repeat
 
--  deal with the connection
runConn :: (Socket, SockAddr) -> Fiber ()
runConn (sock, _) = do
    m <- recv sock 100
    send sock m
    close sock
```
