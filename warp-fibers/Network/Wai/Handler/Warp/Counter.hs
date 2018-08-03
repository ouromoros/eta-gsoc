{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Counter (
    Counter
  , newCounter
  , waitForZero
  , waitForZero'
  , increase
  , decrease
  ) where

import Control.Concurrent.STM

import Network.Wai.Handler.Warp.Imports


newtype Counter = Counter (TVar Int)

newCounter :: Fiber Counter
newCounter = liftIO $ Counter <$> newTVarIO 0

waitForZero :: Counter -> Fiber ()
waitForZero (Counter ref) = liftIO $ atomically $ do
    x <- readTVar ref
    unless (x == 0) retry

waitForZero' :: Counter -> IO ()
waitForZero' (Counter ref) = atomically $ do
    x <- readTVar ref
    unless (x == 0) retry

increase :: Counter -> Fiber ()
increase (Counter ref) = liftIO $ atomically $ modifyTVar' ref $ \x -> x + 1

decrease :: Counter -> Fiber ()
decrease (Counter ref) = liftIO $ atomically $ modifyTVar' ref $ \x -> x - 1
