import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar hiding (takeMVar, putMVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import System.Environment
import GHC.Conc.Sync hiding (yield)
import GHC.Conc.IO
import Control.Monad.IO.Class

main=  runFiber$ do 
  r   <-  liftIO newEmptyMVar

  loop r

loop r= do
  putMVar r "hello"
  s <- takeMVar r
  liftIO $  print s
  loop r

-- trampolineFiber x= liftIO $ trampolineIO $ runFiber x