import Control.Concurrent.Fiber
import Control.Concurrent(threadDelay)
import Control.Concurrent.Fiber.MVar
import Control.Monad.IO.Class
import qualified Control.Concurrent.MVar as MVar

import System.TimeIt

pingpong :: String -> MVar () -> MVar () -> Fiber ()
pingpong msg sourceChan sinkChan = go 0
 where go n = do
         takeMVar sourceChan
         --liftIO $ putStrLn $ show n ++ ": " ++ msg
         putMVar sinkChan ()
         go (n + 1)

main :: IO ()
main = for 10 $ timeIt $ do
  pingChan <- MVar.newEmptyMVar
  pongChan <- MVar.newEmptyMVar
  forkFiber $ pingpong "Ping" pingChan pongChan
  forkFiber $ pingpong "Pong" pongChan pingChan
  -- Start the chain from the main thread!
  MVar.putMVar pingChan ()
  -- Wait 1 second
  threadDelay 10000000
  return ()
  where 
  for 0 x= return () 
  for n x= x >> for (n-1)x