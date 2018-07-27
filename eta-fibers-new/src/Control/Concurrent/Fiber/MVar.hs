{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Control.Concurrent.Fiber.MVar
  (MVar, takeMVar, putMVar)
where

import GHC.Base
import Control.Concurrent.Fiber
import Control.Monad.IO.Class
import GHC.MVar (MVar(..))

--unIO (IO x)= x
takeMVar :: MVar a -> Fiber a
takeMVar (MVar m) =   go
  where go = liftIO $ IO  $ \s ->
               case tryTakeMVar# m s of
                 (# s', 0#, _ #) ->
                   case addMVarListener# m s' of
                     s'' -> unFiber (block >> go) s''
                 (# s', _,  a #) ->
                   case awakenMVarListeners# m s' of
                     s'' -> (# s'', a  #)

putMVar :: MVar a -> a -> Fiber ()
putMVar (MVar m) x =   go
  where go = liftIO $ IO  $ \s ->
               case tryPutMVar# m x s of
                 (# s', 0# #) ->
                   case addMVarListener# m s' of
                     s'' -> unFiber (block >> go) s''
                 (# s', _  #) ->
                   case awakenMVarListeners# m s' of
                     s'' -> (# s'', () #)

foreign import prim "eta.fibers.PrimOps.addMVarListener"
  addMVarListener# :: MVar# s a -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.awakenMVarListeners"
  awakenMVarListeners# :: MVar# s a -> State# s -> State# s
