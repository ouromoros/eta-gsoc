{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Wai.Handler.Warp.Timeout (
  -- ** Types
    Manager
  , TimeoutAction
  , Handle
  -- ** Manager
  , initialize
  , stopManager
  , killManager
  , withManager
  -- ** Registration
  , register
  , registerKillThread
  -- ** Control
  , tickle
  , cancel
  , pause
  , resume
  -- ** Exceptions
  , TimeoutThread (..)
  ) where

import Control.Concurrent (myThreadId)
import qualified Control.Exception as E
import Control.Concurrent.Fiber
import Control.Reaper
import Data.Typeable (Typeable)
import Data.IORef (IORef)
import qualified Data.IORef as I
import Network.Wai.Handler.Warp.Fiber

----------------------------------------------------------------

-- | A timeout manager
type Manager = Reaper [Handle] Handle

-- | An action to be performed on timeout.
type TimeoutAction = Fiber ()

-- | A handle used by 'Manager'
data Handle = Handle !(IORef TimeoutAction) !(IORef State)

data State = Active    -- Manager turns it to Inactive.
           | Inactive  -- Manager removes it with timeout action.
           | Paused    -- Manager does not change it.
           | Canceled  -- Manager removes it without timeout action.

----------------------------------------------------------------

-- | Creating timeout manager which works every N micro seconds
--   where N is the first argument.
initialize :: Int -> Fiber Manager
initialize timeout = liftIO $ mkReaper defaultReaperSettings
        { reaperAction = mkListAction prune
        , reaperDelay = timeout
        }
  where
    prune m@(Handle actionRef stateRef) = do
        state <- I.atomicModifyIORef' stateRef (\x -> (inactivate x, x))
        case state of
            Inactive -> do
                onTimeout <- I.readIORef actionRef
                (fiber onTimeout) `E.catch` (fiber . ignoreAll)
                return Nothing
            Canceled -> return Nothing
            _        -> return $ Just m

    inactivate Active = Inactive
    inactivate x = x

----------------------------------------------------------------

-- | Stopping timeout manager with onTimeout fired.
stopManager :: Manager -> Fiber ()
stopManager mgr = liftIO $ E.mask_ (reaperStop mgr >>= mapM_ fire)
  where
    fire (Handle actionRef _) = do
        onTimeout <- I.readIORef actionRef
        (fiber onTimeout) `E.catch` (fiber . ignoreAll)

ignoreAll :: E.SomeException -> Fiber ()
ignoreAll _ = return ()

-- | Killing timeout manager immediately without firing onTimeout.
killManager :: Manager -> Fiber ()
killManager = liftIO . reaperKill

----------------------------------------------------------------

-- | Registering a timeout action.
register :: Manager -> TimeoutAction -> Fiber Handle
register mgr onTimeout = liftIO $ do
    actionRef <- I.newIORef onTimeout
    stateRef  <- I.newIORef Active
    let h = Handle actionRef stateRef
    reaperAdd mgr h
    return h

-- | Registering a timeout action of killing this thread.
registerKillThread = undefined
-- registerKillThread :: Manager -> TimeoutAction -> Fiber Handle
-- registerKillThread m onTimeout = do
--     -- If we hold ThreadId, the stack and data of the thread is leaked.
--     -- If we hold Weak ThreadId, the stack is released. However, its
--     -- data is still leaked probably because of a bug of GHC.
--     -- So, let's just use ThreadId and release ThreadId by
--     -- overriding the timeout action by "cancel".
--     tid <- liftIO myThreadId
--     -- First run the timeout action in case the child thread is masked.
--     register m (liftIO $ (fiber onTimeout) `E.finally` E.throwTo tid TimeoutThread)

data TimeoutThread = TimeoutThread
    deriving Typeable
instance E.Exception TimeoutThread where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException
instance Show TimeoutThread where
    show TimeoutThread = "Thread killed by Warp's timeout reaper"

----------------------------------------------------------------

-- | Setting the state to active.
--   'Manager' turns active to inactive repeatedly.
tickle :: Handle -> Fiber ()
tickle (Handle _ stateRef) = liftIO $ I.writeIORef stateRef Active

-- | Setting the state to canceled.
--   'Manager' eventually removes this without timeout action.
cancel :: Handle -> Fiber ()
cancel (Handle actionRef stateRef) = liftIO $ do
    I.writeIORef actionRef (return ()) -- ensuring to release ThreadId
    I.writeIORef stateRef Canceled

-- | Setting the state to paused.
--   'Manager' does not change the value.
pause :: Handle -> Fiber ()
pause (Handle _ stateRef) = liftIO $ I.writeIORef stateRef Paused

-- | Setting the paused state to active.
--   This is an alias to 'tickle'.
resume :: Handle -> Fiber ()
resume = tickle

----------------------------------------------------------------

-- | Call the inner function with a timeout manager.
withManager :: Int -- ^ timeout in microseconds
            -> (Manager -> Fiber a)
            -> Fiber a
withManager timeout f = do
    -- FIXME when stopManager is available, use it
    man <- initialize timeout
    f man
