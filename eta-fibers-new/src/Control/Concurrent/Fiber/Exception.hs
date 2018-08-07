{-# LANGUAGE CPP, GHCForeignImportPrim, MagicHash, UnboxedTuples,UnliftedFFITypes, MultiParamTypeClasses, ExistentialQuantification, ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances, RankNTypes #-}
module Control.Concurrent.Fiber.Exception
where

import Control.Concurrent.Fiber
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as E
import qualified GHC.IO as IO
import Unsafe.Coerce
import System.IO.Unsafe
import GHC.Base


foreign import prim "eta.fibers.PrimOps.catchFiber_"
  catchFiber# :: Any -> Any -> Any -> State# s -> (# State# s, Any #)

-- HACK ATTENTION: add 'yield' before f so that outer continuation is stopped
-- catch :: E.Exception e => Fiber a -> (e -> Fiber a) -> Fiber a
-- catch f g = callCC $ \k -> liftIO $ runFiber (yield >> f)  `E.catch` (\e -> runFiber (g e >>= k))
catch :: E.Exception e => Fiber a -> (e -> Fiber a) -> Fiber a
catch f g = callCC $ \k -> liftIO $ catchFiber (runFiber $ yield >> f) (\e -> runFiber (g e)) (runFiber . k)
  where
    catchFiber f g k = IO $ \s -> case catchFiber# (unsafeCoerce f) (unsafeCoerce g) (unsafeCoerce k) s
                                    of (# s', x #) -> (# s', unsafeCoerce x #)

-- catch' :: Fiber a -> (a -> Fiber b) -> Fiber b
-- catch' f g = (catch f $ \e -> liftIO $ E.throw (e :: E.SomeException)) >>= g


onException :: Fiber a -> Fiber b -> Fiber a
onException io what = io `catch` \e -> do _ <- what
                                          liftIO $ E.throwIO (e :: E.SomeException)

bracket :: Fiber a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> Fiber b)  -- ^ computation to run last (\"release resource\")
        -> (a -> Fiber c)  -- ^ computation to run in-between
        -> Fiber c         -- returns the value from the in-between computation
bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

finally :: Fiber a         -- ^ computation to run first
        -> Fiber b         -- ^ computation to run afterward (even if an exception
                           -- was raised)
        -> Fiber a         -- returns the value from the first computation
a `finally` sequel =
  mask $ \restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r

try :: E.Exception e => Fiber a -> Fiber (Either e a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))

handle     :: E.Exception e => (e -> Fiber a) -> Fiber a -> Fiber a
handle     =  flip catch

bracketOnError
        :: Fiber a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> Fiber b)  -- ^ computation to run last (\"release resource\")
        -> (a -> Fiber c)  -- ^ computation to run in-between
        -> Fiber c         -- returns the value from the in-between computation
bracketOnError before after thing =
  mask $ \restore -> do
    a <- before
    restore (thing a) `onException` after a


blockAsync :: Fiber a -> Fiber a
blockAsync f = callCC $ \k -> liftIO $ block' (runFiber f) (unFiber . k)
block' (IO io) k = IO $ \s -> case maskAsyncExceptions# io s of
                                            (# s', a #) -> case k a s' of
                                              (# s'', b #) -> (# s'', unsafeCoerce b #)

-- To re-enable asynchronous exceptions inside the scope of
-- 'block', 'unblock' can be
-- used.  It scopes in exactly the same way, so on exit from
-- 'unblock' asynchronous exception delivery will
-- be disabled again.
unblockAsync :: Fiber a -> Fiber a
unblockAsync = unsafeUnmask

unsafeUnmask :: Fiber a -> Fiber a
unsafeUnmask f = callCC $ \k -> liftIO $ unsafeUnmask' (runFiber f) (unFiber . k)
unsafeUnmask' (IO io) k = IO $ \s -> case unmaskAsyncExceptions# io s of
                                            (# s', a #) -> case k a s' of
                                              (# s'', b #) -> (# s'', unsafeCoerce b #)

blockUninterruptible :: Fiber a -> Fiber a
blockUninterruptible f = callCC $ \k -> liftIO $ blockUninterruptible' (runFiber f) (unFiber . k)
blockUninterruptible' (IO io) k = IO $ \s -> case maskUninterruptible# io s of
                                            (# s', a #) -> case k a s' of
                                              (# s'', b #) -> (# s'', unsafeCoerce b #)

mask  :: ((forall a. Fiber a -> Fiber a) -> Fiber b) -> Fiber b
mask io = do
  b <- liftIO $ IO.getMaskingState
  case b of
    IO.Unmasked              -> blockAsync $ io unblockAsync
    IO.MaskedInterruptible   -> io blockAsync
    IO.MaskedUninterruptible -> io blockUninterruptible

mask_ :: Fiber a -> Fiber a
mask_ io = mask $ \_ -> io
