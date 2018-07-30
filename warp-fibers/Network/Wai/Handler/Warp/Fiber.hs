
module Network.Wai.Handler.Warp.Fiber (
    liftIO
  , forkFiberAndWait
  , fiber
  , readMVar
  , throwFiber
  ) where


import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import GHC.IO (IO(..))
import qualified Control.Concurrent.MVar as IM
import Control.Monad.IO.Class (liftIO)
import Control.Exception as E
import GHC.Conc.Sync (ThreadId(..))

forkFiberAndWait :: Fiber a -> IO ()
forkFiberAndWait f = do
    m <- IM.newEmptyMVar 
    -- should use something like `finally` instead of >> here
    forkFiber (f >> (liftIO $ IM.putMVar m ()))
    IM.takeMVar m
    return ()

fiber :: Fiber a -> IO a
fiber = runFiber

throwFiber :: E.Exception e => FiberId -> e -> Fiber ()
throwFiber (FiberId id) = liftIO . E.throwTo (ThreadId id)

readMVar :: MVar a -> Fiber a
readMVar = liftIO . IM.readMVar