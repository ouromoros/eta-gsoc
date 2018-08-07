import Control.Concurrent.Fiber
import Data.IORef
import Control.Monad.IO.Class
import Control.Applicative
import Data.Monoid
import Control.Concurrent



main= do
   r <- liftIO $ newIORef 0
   sum  r 1000000
 where
 sum r 0= do r <- liftIO $ readIORef r; liftIO $ print r
 sum r x= do
    liftIO $ modifyIORef r $ \v -> v + x 
    sum r $x -1 