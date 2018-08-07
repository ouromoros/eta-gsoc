{-# LANGUAGE CPP, BangPatterns #-}

-- | A thread pool manager.
--   The manager has responsibility to spawn and kill
--   worker threads.
module Network.Wai.Handler.Warp.HTTP2.Manager (
    Manager
  , start
  , setAction
  , stop
  , spawnAction
  , addMyId
  , deleteMyId
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Settings
import qualified Network.Wai.Handler.Warp.Timeout as T

----------------------------------------------------------------

type Action = T.Manager -> Fiber ()

data Command = Stop | Spawn | Add ThreadId | Delete ThreadId

data Manager = Manager (TQueue Command) (IORef Action)

-- | Starting a thread pool manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: Settings -> Fiber Manager
start set = do
    q <- liftIO newTQueueIO
    ref <- liftIO $ newIORef (\_ -> return ())
    timmgr <- T.initialize $ settingsTimeout set * 1000000
    liftIO $ forkFiber $ go q Set.empty ref timmgr
    return $ Manager q ref
  where
    go q !tset0 ref timmgr = do
        x <- liftIO $ atomically $ readTQueue q
        case x of
            Stop          -> kill tset0 >> T.killManager timmgr
            Spawn         -> next tset0
            Add    newtid -> let !tset = add newtid tset0
                             in go q tset ref timmgr
            Delete oldtid -> let !tset = del oldtid tset0
                             in go q tset ref timmgr
      where
        next tset = do
            action <- liftIO $ readIORef ref
            newtid <- forkFiber (action timmgr)
            let !tset' = add (toThreadId newtid) tset
            go q tset' ref timmgr

setAction :: Manager -> Action -> Fiber ()
setAction (Manager _ ref) action = liftIO $ writeIORef ref action

stop :: Manager -> Fiber ()
stop (Manager q _) = liftIO $ atomically $ writeTQueue q Stop

spawnAction :: Manager -> Fiber ()
spawnAction (Manager q _) = liftIO $ atomically $ writeTQueue q Spawn

addMyId :: Manager -> Fiber ()
addMyId (Manager q _) = liftIO $ do
    tid <- myThreadId
    atomically $ writeTQueue q $ Add tid

deleteMyId :: Manager -> Fiber ()
deleteMyId (Manager q _) = liftIO $ do
    tid <- myThreadId
    atomically $ writeTQueue q $ Delete tid

----------------------------------------------------------------

add :: ThreadId -> Set ThreadId -> Set ThreadId
add tid set = set'
  where
    !set' = Set.insert tid set

del :: ThreadId -> Set ThreadId -> Set ThreadId
del tid set = set'
  where
    !set' = Set.delete tid set

kill :: Set ThreadId -> Fiber ()
kill set = liftIO $ traverse_ killThread set
