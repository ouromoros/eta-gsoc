{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.HTTP2.Worker (
    Responder
  , response
  , worker
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException(..), AsyncException(..))
import qualified Control.Exception as E
import Data.ByteString.Builder (byteString)
import Data.IORef
import qualified Data.Vault.Lazy as Vault
import Network.HPACK
import Network.HPACK.Token
import qualified Network.HTTP.Types as H
import Network.HTTP2
import Network.HTTP2.Priority
import Network.Wai
import qualified Network.Wai.Handler.Warp.Timeout as Timeout
import Network.Wai.Internal (Response(..), ResponseReceived(..), ResponseReceived(..))

import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.File
import Network.Wai.Handler.Warp.HTTP2.Manager
import Network.Wai.Handler.Warp.HTTP2.Request
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Imports hiding (insert)
import Network.Wai.Handler.Warp.Request (pauseTimeoutKey)
import qualified Network.Wai.Handler.Warp.Response as R
import qualified Network.Wai.Handler.Warp.Settings as S
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | The wai definition is 'type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived'.
--   This type implements the second argument (Response -> IO ResponseReceived)
--   with extra arguments.
type Responder = InternalInfo
              -> ValueTable -- for Request
              -> ThreadContinue
              -> Stream
              -> Request
              -> Response
              -> Fiber ResponseReceived

pushStream :: Context -> S.Settings
           -> StreamId -> ValueTable -> Request -> InternalInfo
           -> Maybe HTTP2Data
           -> Fiber (OutputType, Fiber ())
pushStream _ _ _ _ _ _ Nothing = return (ORspn, return ())
pushStream ctx@Context{http2settings,outputQ,streamTable}
           settings pid reqvt req ii (Just h2d)
  | len == 0 = return (ORspn, return ())
  | otherwise = do
        pushable <- liftIO $ enablePush <$> readIORef http2settings
        if pushable then do
            tvar <- liftIO $ newTVarIO 0
            lim <- push tvar pps0 0
            if lim == 0 then
              return (ORspn, return ())
             else
              return (OWait, liftIO $ waiter lim tvar)
          else
            return (ORspn, return ())
  where
    !pps0 = http2dataPushPromise h2d
    !len = length pps0
    !pushLogger = S.settingsServerPushLogger settings
    increment tvar = atomically $ modifyTVar' tvar (+1)
    waiter lim tvar = atomically $ do
        n <- readTVar tvar
        check (n >= lim)
    !h2data = getHTTP2Data req
    push _ [] !n = return (n :: Int)
    push tvar (pp:pps) !n = do
        let !file = promisedFile pp
        efinfo <- liftIO $ E.try $ fiber $ getFileInfo ii file
        case efinfo of
          Left (_ex :: E.IOException) -> push tvar pps n
          Right (FileInfo _ size _ date) -> do
              ws <- liftIO $ initialWindowSize <$> readIORef http2settings
              let !w = promisedWeight pp
                  !pri = defaultPriority { weight = w }
                  !pre = toPrecedence pri
              strm <- newPushStream ctx ws pre
              let !sid = streamNumber strm
              insert streamTable sid strm
              (ths0, vt) <- liftIO $ toHeaderTable (promisedResponseHeaders pp)
              let !scheme = fromJust $ getHeaderValue tokenScheme reqvt
                  -- fixme: this value can be Nothing
                  !auth   = fromJust (getHeaderValue tokenHost reqvt
                                  <|> getHeaderValue tokenAuthority reqvt)
                  !path = promisedPath pp
                  !promisedRequest = [(tokenMethod, H.methodGet)
                                     ,(tokenScheme, scheme)
                                     ,(tokenAuthority, auth)
                                     ,(tokenPath, path)]
                  !part = FilePart 0 size size
                  !rsp = RspnFile H.ok200 (ths,vt) file (Just part)
                  !ths = (tokenLastModified,date) :
                         addContentHeadersForFilePart ths0 part
              pushLogger req path size
              let !ot = OPush promisedRequest pid
                  !out = Output strm rsp ii (liftIO $ increment tvar) h2data ot
              enqueueOutput outputQ out
              push tvar pps (n + 1)


-- | This function is passed to workers.
--   They also pass 'Response's from 'Application's to this function.
--   This function enqueues commands for the HTTP/2 sender.
response :: S.Settings -> Context -> Manager -> Responder
response settings ctx@Context{outputQ} mgr ii reqvt tconf strm req rsp = case rsp of
  ResponseStream s0 hs0 strmbdy
    | noBody s0          -> responseNoBody s0 hs0
    | isHead             -> responseNoBody s0 hs0
    | otherwise          -> getHTTP2Data req
                        >>= pushStream ctx settings sid reqvt req ii
                        >>= responseStreaming s0 hs0 strmbdy
  ResponseBuilder s0 hs0 b
    | noBody s0          -> responseNoBody s0 hs0
    | isHead             -> responseNoBody s0 hs0
    | otherwise          -> getHTTP2Data req
                        >>= pushStream ctx settings sid reqvt req ii
                        >>= responseBuilderBody s0 hs0 b
  ResponseFile s0 hs0 p mp
    | noBody s0          -> responseNoBody s0 hs0
    | otherwise          -> getHTTP2Data req
                        >>= pushStream ctx settings sid reqvt req ii
                        >>= responseFileXXX s0 hs0 p mp
  ResponseRaw _ _        -> error "HTTP/2 does not support ResponseRaw"
  where
    noBody = not . R.hasBody
    !isHead = requestMethod req == H.methodHead
    !logger = S.settingsLogger settings
    !th = threadHandle ii
    sid = streamNumber strm
    !h2data = getHTTP2Data req

    -- Ideally, log messages should be written when responses are
    -- actually sent. But there is no way to keep good memory usage
    -- (resist to Request leak) and throughput. By compromise,
    -- log message are written here even the window size of streams
    -- is 0.

    responseNoBody s hs0 = (liftIO $ toHeaderTable hs0) >>= responseNoBody' s

    responseNoBody' s tbl = do
        logger req s Nothing
        setThreadContinue tconf True
        let !rspn = RspnNobody s tbl
            !out = Output strm rspn ii (return ()) h2data ORspn
        enqueueOutput outputQ out
        return ResponseReceived

    responseBuilderBody s hs0 bdy (rspnOrWait,tell) = do
        logger req s Nothing
        setThreadContinue tconf True
        tbl <- liftIO $ toHeaderTable hs0
        let !rspn = RspnBuilder s tbl bdy
            !out = Output strm rspn ii tell h2data rspnOrWait
        enqueueOutput outputQ out
        return ResponseReceived

    responseFileXXX _ hs0 path Nothing aux = do
        efinfo <- liftIO $ E.try $ fiber $ getFileInfo ii path
        case efinfo of
            Left (_ex :: E.IOException) -> response404 hs0
            Right finfo -> do
                (rspths0,vt) <- liftIO $ toHeaderTable hs0
                case conditionalRequest finfo rspths0 reqvt of
                    WithoutBody s             -> responseNoBody s hs0
                    WithBody s rspths beg len -> responseFile2XX s (rspths,vt) path (Just (FilePart beg len (fileInfoSize finfo))) aux

    responseFileXXX s0 hs0 path mpart aux = do
        tbl <- liftIO $ toHeaderTable hs0
        responseFile2XX s0 tbl path mpart aux

    responseFile2XX s tbl path mpart (rspnOrWait,tell)
      | isHead    = do
          logger req s Nothing
          responseNoBody' s tbl
      | otherwise = do
          logger req s (filePartByteCount <$> mpart)
          setThreadContinue tconf True
          let !rspn = RspnFile s tbl path mpart
              !out = Output strm rspn ii tell h2data rspnOrWait
          enqueueOutput outputQ out
          return ResponseReceived

    response404 hs0 = responseBuilderBody s hs body (ORspn, return ())
      where
        s = H.notFound404
        hs = R.replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
        body = byteString "File not found"

    responseStreaming s0 hs0 strmbdy (rspnOrWait,tell) = do
        logger req s0 Nothing
        -- We must not exit this WAI application.
        -- If the application exits, streaming would be also closed.
        -- So, this work occupies this thread.
        --
        -- We need to increase the number of workers.
        spawnAction mgr
        -- After this work, this thread stops to decease
        -- the number of workers.
        setThreadContinue tconf False
        -- Since 'StreamingBody' is loop, we cannot control it.
        -- So, let's serialize 'Builder' with a designated queue.
        tbq <- liftIO $ newTBQueueIO 10 -- fixme: hard coding: 10
        tbl <- liftIO $ toHeaderTable hs0
        let !rspn = RspnStreaming s0 tbl tbq
            !out = Output strm rspn ii tell h2data rspnOrWait
        enqueueOutput outputQ out
        let push b = do
              T.pause th
              liftIO $ atomically $ writeTBQueue tbq (SBuilder b)
              T.resume th
            flush  = atomically $ writeTBQueue tbq SFlush
        _ <- liftIO $ strmbdy (fiber . push) flush
        liftIO $ atomically $ writeTBQueue tbq SFinish
        deleteMyId mgr
        return ResponseReceived

worker :: Context -> S.Settings -> Application -> Responder -> T.Manager -> Fiber ()
worker ctx@Context{inputQ,controlQ} set app responder tm = do
    sinfo <- newStreamInfo
    tcont <- newThreadContinue
    let timeoutAction = return () -- cannot close the shared connection
    liftIO $ E.bracket (fiber $ T.registerKillThread tm timeoutAction) (fiber . T.cancel) (fiber . go sinfo tcont)
  where
    go sinfo tcont th = do
        setThreadContinue tcont True
        ex <- liftIO $ E.try $ fiber $ do
            T.pause th
            inp@(Input strm req reqvt ii) <- liftIO $ atomically $ readTQueue inputQ
            setStreamInfo sinfo inp
            T.resume th
            T.tickle th
            let !ii' = ii { threadHandle = th }
                !body = liftIO $ requestBody req
                !body' = do
                    T.pause th
                    bs <- body
                    T.resume th
                    return bs
                !vaultValue = Vault.insert pauseTimeoutKey (Timeout.pause th) $ vault req
                !req' = req { vault = vaultValue, requestBody = (fiber body') }
            liftIO $ app req' (fiber . responder ii' reqvt tcont strm req')
        cont1 <- case ex of
            Right ResponseReceived -> return True
            Left  e@(SomeException _)
              -- killed by the local worker manager
              | Just ThreadKilled    <- E.fromException e -> return False
              -- killed by the local timeout manager
              | Just T.TimeoutThread <- E.fromException e -> do
                  cleanup sinfo Nothing
                  return True
              | otherwise -> do
                  cleanup sinfo $ Just e
                  return True
        cont2 <- getThreadContinue tcont
        clearStreamInfo sinfo
        when (cont1 && cont2) $ go sinfo tcont th
    cleanup sinfo me = do
        minp <- getStreamInfo sinfo
        case minp of
            Nothing               -> return ()
            Just (Input strm req _reqvt _ii) -> do
                closed ctx strm Killed
                let !frame = resetFrame InternalError (streamNumber strm)
                enqueueControl controlQ $ CFrame frame
                case me of
                    Nothing -> return ()
                    Just e  -> S.settingsOnException set (Just req) e

----------------------------------------------------------------

-- | It would nice if responders could return values to workers.
--   Unfortunately, 'ResponseReceived' is already defined in WAI 2.0.
--   It is not wise to change this type.
--   So, a reference is shared by a responder and its worker.
--   The reference refers a value of this type as a return value.
--   If 'True', the worker continue to serve requests.
--   Otherwise, the worker get finished.
newtype ThreadContinue = ThreadContinue (IORef Bool)

{-# INLINE newThreadContinue #-}
newThreadContinue :: Fiber ThreadContinue
newThreadContinue = liftIO $ ThreadContinue <$> newIORef True

{-# INLINE setThreadContinue #-}
setThreadContinue :: ThreadContinue -> Bool -> Fiber ()
setThreadContinue (ThreadContinue ref) x = liftIO $ writeIORef ref x

{-# INLINE getThreadContinue #-}
getThreadContinue :: ThreadContinue -> Fiber Bool
getThreadContinue (ThreadContinue ref) = liftIO $ readIORef ref

----------------------------------------------------------------

-- | The type to store enough information for 'settingsOnException'.
newtype StreamInfo = StreamInfo (IORef (Maybe Input))

{-# INLINE newStreamInfo #-}
newStreamInfo :: Fiber StreamInfo
newStreamInfo = liftIO $ StreamInfo <$> newIORef Nothing

{-# INLINE clearStreamInfo #-}
clearStreamInfo :: StreamInfo -> Fiber ()
clearStreamInfo (StreamInfo ref) = liftIO $ writeIORef ref Nothing

{-# INLINE setStreamInfo #-}
setStreamInfo :: StreamInfo -> Input -> Fiber ()
setStreamInfo (StreamInfo ref) inp = liftIO $ writeIORef ref $ Just inp

{-# INLINE getStreamInfo #-}
getStreamInfo :: StreamInfo -> Fiber (Maybe Input)
getStreamInfo (StreamInfo ref) = liftIO $ readIORef ref
