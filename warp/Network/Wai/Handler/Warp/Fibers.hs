
module Network.Wai.Handler.Warp.Fibers where

import Control.Concurrent.Fiber
fiber :: Fiber a -> IO a
fiber (Fiber a) = IO a