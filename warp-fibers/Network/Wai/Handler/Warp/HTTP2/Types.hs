{-# LANGUAGE OverloadedStrings, CPP #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.HTTP2.Types where

-- import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.Fiber.Exception (bracket)
import Control.Exception (SomeException)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import Data.IORef
import Data.IntMap.Strict (IntMap, IntMap)
import qualified Data.IntMap.Strict as M
import Network.HPACK hiding (Buffer)
import qualified Network.HTTP.Types as H
import Network.HTTP2
import Network.HTTP2.Priority
import Network.Wai (FilePart)

import Network.Wai.Handler.Warp.HTTP2.Manager
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

http2ver :: H.HttpVersion
http2ver = H.HttpVersion 2 0

isHTTP2 :: Transport -> Bool
isHTTP2 TCP = False
isHTTP2 tls = useHTTP2
  where
    useHTTP2 = case tlsNegotiatedProtocol tls of
        Nothing    -> False
        Just proto -> "h2-" `BS.isPrefixOf` proto

----------------------------------------------------------------

data Input = Input Stream Request ValueTable InternalInfo

----------------------------------------------------------------

type DynaNext = Buffer -> BufSize -> WindowSize -> Fiber Next

type BytesFilled = Int

data Next = Next !BytesFilled (Maybe DynaNext)

data Rspn = RspnNobody    H.Status (TokenHeaderList, ValueTable)
          | RspnStreaming H.Status (TokenHeaderList, ValueTable) (TBQueue Sequence)
          | RspnBuilder   H.Status (TokenHeaderList, ValueTable) Builder
          | RspnFile      H.Status (TokenHeaderList, ValueTable) FilePath (Maybe FilePart)

rspnStatus :: Rspn -> H.Status
rspnStatus (RspnNobody    s _)      = s
rspnStatus (RspnStreaming s _ _)    = s
rspnStatus (RspnBuilder   s _ _)    = s
rspnStatus (RspnFile      s _ _ _ ) = s

rspnHeaders :: Rspn -> (TokenHeaderList, ValueTable)
rspnHeaders (RspnNobody    _ t)      = t
rspnHeaders (RspnStreaming _ t _)    = t
rspnHeaders (RspnBuilder   _ t _)    = t
rspnHeaders (RspnFile      _ t _ _ ) = t

data Output = Output {
    outputStream :: !Stream
  , outputRspn   :: !Rspn
  , outputII     :: !InternalInfo
  , outputHook   :: Fiber () -- OPush: wait for done, O*: telling done
  , outputH2Data :: Fiber (Maybe HTTP2Data)
  , outputType   :: !OutputType
  }

data OutputType = ORspn
                | OWait
                | OPush !TokenHeaderList !StreamId -- associated stream id from client
                | ONext !DynaNext

outputMaybeTBQueue :: Output -> Maybe (TBQueue Sequence)
outputMaybeTBQueue (Output _ (RspnStreaming _ _ tbq) _ _ _ _) = Just tbq
outputMaybeTBQueue _                                          = Nothing

data Control = CFinish
             | CGoaway    !ByteString
             | CFrame     !ByteString
             | CSettings  !ByteString !SettingsList
             | CSettings0 !ByteString !ByteString !SettingsList

----------------------------------------------------------------

data Sequence = SFinish
              | SFlush
              | SBuilder Builder

----------------------------------------------------------------

-- | The context for HTTP/2 connection.
data Context = Context {
  -- HTTP/2 settings received from a browser
    http2settings      :: !(IORef Settings)
  , firstSettings      :: !(IORef Bool)
  , streamTable        :: !StreamTable
  , concurrency        :: !(IORef Int)
  , priorityTreeSize   :: !(IORef Int)
  -- | RFC 7540 says "Other frames (from any stream) MUST NOT
  --   occur between the HEADERS frame and any CONTINUATION
  --   frames that might follow". This field is used to implement
  --   this requirement.
  , continued          :: !(IORef (Maybe StreamId))
  , clientStreamId     :: !(IORef StreamId)
  , serverStreamId     :: !(IORef StreamId)
  , inputQ             :: !(TQueue Input)
  , outputQ            :: !(PriorityTree Output)
  , controlQ           :: !(TQueue Control)
  , encodeDynamicTable :: !DynamicTable
  , decodeDynamicTable :: !DynamicTable
  -- the connection window for data from a server to a browser.
  , connectionWindow   :: !(TVar WindowSize)
  }

----------------------------------------------------------------

newContext :: Fiber Context
newContext = liftIO $ Context <$> newIORef defaultSettings
                     <*> newIORef False
                     <*> newStreamTable
                     <*> newIORef 0
                     <*> newIORef 0
                     <*> newIORef Nothing
                     <*> newIORef 0
                     <*> newIORef 0
                     <*> newTQueueIO
                     <*> newPriorityTree
                     <*> newTQueueIO
                     <*> newDynamicTableForEncoding defaultDynamicTableSize
                     <*> newDynamicTableForDecoding defaultDynamicTableSize 4096
                     <*> newTVarIO defaultInitialWindowSize

clearContext :: Context -> Fiber ()
clearContext _ctx = return ()

----------------------------------------------------------------

data OpenState =
    JustOpened
  | Continued [HeaderBlockFragment]
              !Int  -- Total size
              !Int  -- The number of continuation frames
              !Bool -- End of stream
              !Priority
  | NoBody (TokenHeaderList,ValueTable) !Priority
  | HasBody (TokenHeaderList,ValueTable) !Priority
  | Body !(TQueue ByteString)
         !(Maybe Int) -- received Content-Length
                      -- compared the body length for error checking
         !(IORef Int) -- actual body length

data ClosedCode = Finished
                | Killed
                | Reset !ErrorCodeId
                | ResetByMe SomeException
                deriving Show

data StreamState =
    Idle
  | Open !OpenState
  | HalfClosed
  | Closed !ClosedCode
  | Reserved

isIdle :: StreamState -> Bool
isIdle Idle = True
isIdle _    = False

isOpen :: StreamState -> Bool
isOpen Open{} = True
isOpen _      = False

isHalfClosed :: StreamState -> Bool
isHalfClosed HalfClosed = True
isHalfClosed _          = False

isClosed :: StreamState -> Bool
isClosed Closed{} = True
isClosed _        = False

instance Show StreamState where
    show Idle        = "Idle"
    show Open{}      = "Open"
    show HalfClosed  = "HalfClosed"
    show (Closed e)  = "Closed: " ++ show e
    show Reserved    = "Reserved"

----------------------------------------------------------------

data Stream = Stream {
    streamNumber     :: !StreamId
  , streamState      :: !(IORef StreamState)
  , streamWindow     :: !(TVar WindowSize)
  , streamPrecedence :: !(IORef Precedence)
  }

instance Show Stream where
  show s = show (streamNumber s)

newStream :: StreamId -> WindowSize -> Fiber Stream
newStream sid win = liftIO $ Stream sid <$> newIORef Idle
                               <*> newTVarIO win
                               <*> newIORef defaultPrecedence

newPushStream :: Context -> WindowSize -> Precedence -> Fiber Stream
newPushStream Context{serverStreamId} win pre = liftIO $ do
    sid <- atomicModifyIORef' serverStreamId inc2
    Stream sid <$> newIORef Reserved
               <*> newTVarIO win
               <*> newIORef pre
  where
    inc2 x = let !x' = x + 2 in (x', x')

----------------------------------------------------------------

opened :: Context -> Stream -> Fiber ()
opened Context{concurrency} Stream{streamState} = liftIO $ do
    atomicModifyIORef' concurrency (\x -> (x+1,()))
    writeIORef streamState (Open JustOpened)

closed :: Context -> Stream -> ClosedCode -> Fiber ()
closed Context{concurrency,streamTable} Stream{streamState,streamNumber} cc = do
    remove streamTable streamNumber
    liftIO $ atomicModifyIORef' concurrency (\x -> (x-1,()))
    liftIO $ writeIORef streamState (Closed cc) -- anyway

----------------------------------------------------------------

newtype StreamTable = StreamTable (IORef (IntMap Stream))

newStreamTable :: IO StreamTable
newStreamTable = StreamTable <$> newIORef M.empty

insert :: StreamTable -> M.Key -> Stream -> Fiber ()
insert (StreamTable ref) k v = liftIO $ atomicModifyIORef' ref $ \m ->
    let !m' = M.insert k v m
    in (m', ())

remove :: StreamTable -> M.Key -> Fiber ()
remove (StreamTable ref) k = liftIO $ atomicModifyIORef' ref $ \m ->
    let !m' = M.delete k m
    in (m', ())

search :: StreamTable -> M.Key -> Fiber (Maybe Stream)
search (StreamTable ref) k = liftIO $ M.lookup k <$> readIORef ref

updateAllStreamWindow :: (WindowSize -> WindowSize) -> StreamTable -> Fiber ()
updateAllStreamWindow adst (StreamTable ref) = liftIO $ do
    strms <- M.elems <$> readIORef ref
    forM_ strms $ \strm -> atomically $ modifyTVar (streamWindow strm) adst

{-# INLINE forkAndEnqueueWhenReady #-}
forkAndEnqueueWhenReady :: Fiber () -> PriorityTree Output -> Output -> Manager -> Fiber ()
forkAndEnqueueWhenReady wait outQ out mgr = bracket setup teardown $ \_ ->
    void . forkFiber $ do
        wait
        enqueueOutput outQ out
  where
    setup = addMyId mgr
    teardown _ = deleteMyId mgr

{-# INLINE enqueueOutput #-}
enqueueOutput :: PriorityTree Output -> Output -> Fiber ()
enqueueOutput outQ out = do
    let Stream{..} = outputStream out
    pre <- liftIO $ readIORef streamPrecedence
    liftIO $ enqueue outQ streamNumber pre out

{-# INLINE enqueueControl #-}
enqueueControl :: TQueue Control -> Control -> Fiber ()
enqueueControl ctlQ ctl = liftIO $ atomically $ writeTQueue ctlQ ctl

----------------------------------------------------------------

-- | HTTP/2 specific data.
--
--   Since: 3.2.7
data HTTP2Data = HTTP2Data {
    -- | Accessor for 'PushPromise' in 'HTTP2Data'.
    --
    --   Since: 3.2.7
      http2dataPushPromise :: [PushPromise]
    --   Since: 3.2.8
    , http2dataTrailers :: H.ResponseHeaders
    } deriving (Eq,Show)

-- | Default HTTP/2 specific data.
--
--   Since: 3.2.7
defaultHTTP2Data :: HTTP2Data
defaultHTTP2Data = HTTP2Data [] []

-- | HTTP/2 push promise or sever push.
--
--   Since: 3.2.7
data PushPromise = PushPromise {
    -- | Accessor for a URL path in 'PushPromise'.
    --   E.g. \"\/style\/default.css\".
    --
    --   Since: 3.2.7
      promisedPath            :: ByteString
    -- | Accessor for 'FilePath' in 'PushPromise'.
    --   E.g. \"FILE_PATH/default.css\".
    --
    --   Since: 3.2.7
    , promisedFile            :: FilePath
    -- | Accessor for 'H.ResponseHeaders' in 'PushPromise'
    --   \"content-type\" must be specified.
    --   Default value: [].
    --
    --
    --   Since: 3.2.7
    , promisedResponseHeaders :: H.ResponseHeaders
    -- | Accessor for 'Weight' in 'PushPromise'.
    --    Default value: 16.
    --
    --   Since: 3.2.7
    , promisedWeight          :: Weight
    } deriving (Eq,Ord,Show)

-- | Default push promise.
--
--   Since: 3.2.7
defaultPushPromise :: PushPromise
defaultPushPromise = PushPromise "" "" [] 16
