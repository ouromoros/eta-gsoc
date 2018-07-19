
module Network.Wai.Handler.Warp.Fiber where

import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import GHC.IO (IO(..))
import qualified Control.Concurrent.MVar as IM

forkFiberAndWait :: Fiber a -> IO ()
forkFiberAndWait f = do
    m <- IM.newEmptyMVar 
    -- should use something like `finally` instead of >> here
    forkFiber (f >> (liftIO $ IM.putMVar m ()))
    IM.takeMVar m
    return ()

fiber :: Fiber a -> IO a
fiber (Fiber a) = IO a

readMVar :: MVar a -> Fiber a
readMVar m = do
  a <- takeMVar m
  putMVar m a
  return a