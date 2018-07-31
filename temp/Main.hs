module Main where

import Control.Concurrent.Fiber
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar as IM
import Control.Concurrent (forkIO)

forkIOAndWait :: IO a -> IO ()
forkIOAndWait io = do
    m <- IM.newEmptyMVar 
    forkIO (io >> (IM.putMVar m ()))
    IM.takeMVar m
    return ()

main :: IO ()
main = forkIOAndWait main'

main' :: IO ()
main' = runFiber $ do
    yield
    liftIO $ putStrLn "hey, I'm back"
    return ()
