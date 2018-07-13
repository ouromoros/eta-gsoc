
module Network.Wai.Handler.Warp.Fiber where

import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import GHC.IO

fiber :: Fiber a -> IO a
fiber (Fiber a) = IO a

readMVar :: MVar a -> Fiber a
readMVar m = do
  a <- takeMVar m
  putMVar m a
  return a