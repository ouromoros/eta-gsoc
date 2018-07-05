
module Network.Wai.Handler.Warp.Fiber where

import Control.Concurrent.Fiber
import GHC.IO
fiber :: Fiber a -> IO a
fiber (Fiber a) = IO a