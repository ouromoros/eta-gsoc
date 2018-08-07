module Network.Wai.Handler.Warp.Fiber (
    liftIO
  , forkFiberAndWait
  , fiber
  , readMVar
  , killFiber
  , toThreadId
  ) where


import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import GHC.IO (IO(..))
import qualified Control.Concurrent.MVar as IM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Fiber.Exception as E
import qualified Control.Concurrent as C
import qualified Control.Exception as IE
import GHC.Conc.Sync (ThreadId(..))
import Control.Monad (void)

-- | Fork Fiber and wait on it, usually used in main thread to avoid terminating the program
forkFiberAndWait :: Fiber a -> IO ()
forkFiberAndWait f = do
    m <- IM.newEmptyMVar 
    -- should use something like `finally` instead of >> here
    forkFiber ((void f) `E.finally` (liftIO $ IM.putMVar m ()))
    IM.takeMVar m
    return ()

-- | Not safe! Can only be used if you are sure there's no `yield` or `block` in the Fiber monad
fiber :: Fiber a -> IO a
fiber = runFiber

toThreadId :: FiberId -> ThreadId
toThreadId (FiberId id) = ThreadId id

throwTo :: IE.Exception e => FiberId -> e -> Fiber ()
throwTo (FiberId id) = liftIO . C.throwTo (ThreadId id)

killFiber :: FiberId -> Fiber ()
killFiber tid = throwTo tid IE.ThreadKilled
