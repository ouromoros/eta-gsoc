# fibers-network

`fibers-network` implements non-blocking network IO for eta-fibers, and contains most basic functions from `network`. It essentially has the same API with `network` but uses `Fiber` monad instead.

Aside from functions that resemble those in `network`, `fibers-network` also contains some more primitive functions for `Fiber` like `threadWait*`. Basic network IO functions like `bindport*` and `readRawBufferPtr`/`writeRawBufferPtr` are also included. As a matter of principle, they don't differ much from their original version except they're using `Fiber` monad and are non-blocking.

The `Socket` type in `fibers-network` is imported from `network`, so you can also mix blocking functions imported from `network` on the obtained sockets if you want. This can be convenient if you are using libraries that performs IO operations but does not support Fiber yet.

Here is an echo-server example for `fibers-network`:

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
