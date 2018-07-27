
module Network.Wai.Handler.Warp.Fiber (
    liftIO
  , forkFiberAndWait
  , fiber
  , readMVar
  ) where


import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import GHC.IO (IO(..))
import qualified Control.Concurrent.MVar as IM
import Control.Monad.IO.Class (liftIO)

forkFiberAndWait :: Fiber a -> IO ()
forkFiberAndWait f = do
    m <- IM.newEmptyMVar 
    -- should use something like `finally` instead of >> here
    forkFiber (f >> (liftIO $ IM.putMVar m ()))
    IM.takeMVar m
    return ()

fiber :: Fiber a -> IO a
fiber = runFiber

readMVar :: MVar a -> Fiber a
readMVar m = do
  a <- takeMVar m
  putMVar m a
  return a