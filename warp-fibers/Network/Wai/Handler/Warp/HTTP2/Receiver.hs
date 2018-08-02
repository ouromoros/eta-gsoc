{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}

module Network.Wai.Handler.Warp.HTTP2.Receiver (frameReceiver) where

-- import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Concurrent.Fiber.Exception as E
import qualified Control.Exception as IE
import qualified Data.ByteString as BS
import Data.IORef
import Network.HPACK
import Network.HPACK.Token
import Network.HTTP2
import Network.HTTP2.Priority (toPrecedence, delete, prepare)

import Network.Wai.Handler.Warp.HTTP2.EncodeFrame
import Network.Wai.Handler.Warp.HTTP2.HPACK
import Network.Wai.Handler.Warp.HTTP2.Request
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Imports hiding (delete, insert, readInt)
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

frameReceiver :: Context -> MkReq -> (BufSize -> Fiber ByteString) -> Fiber ()
frameReceiver ctx mkreq recvN = loop 0 `E.catch` sendGoaway
  where
    Context{ http2settings
           , streamTable
           , concurrency
           , continued
           , clientStreamId
           , inputQ
           , controlQ
           } = ctx
    sendGoaway e
      | Just (ConnectionError err msg) <- IE.fromException e = do
          csid <- liftIO $ readIORef clientStreamId
          let !frame = goawayFrame csid err msg
          enqueueControl controlQ $ CGoaway frame
      | otherwise = return ()

    sendReset err sid = do
        let !frame = resetFrame err sid
        enqueueControl controlQ $ CFrame frame

    loop :: Int -> Fiber ()
    loop !n
      | n == 6 = do
          yield
          loop 0
      | otherwise = do
        hd <- recvN frameHeaderLength
        if BS.null hd then
            enqueueControl controlQ CFinish
          else do
            cont <- processStreamGuardingError $ decodeFrameHeader hd
            when cont $ loop (n + 1)

    processStreamGuardingError (fid, FrameHeader{streamId})
      | isResponse streamId &&
        (fid `notElem` [FramePriority,FrameRSTStream,FrameWindowUpdate]) =
        liftIO $ IE.throwIO $ ConnectionError ProtocolError "stream id should be odd"
    processStreamGuardingError (FrameUnknown _, FrameHeader{payloadLength}) = do
        mx <- liftIO $ readIORef continued
        case mx of
            Nothing -> do
                -- ignoring unknown frame
                consume payloadLength
                return True
            Just _  -> liftIO $ IE.throwIO $ ConnectionError ProtocolError "unknown frame"
    processStreamGuardingError (FramePushPromise, _) =
        liftIO $ IE.throwIO $ ConnectionError ProtocolError "push promise is not allowed"
    processStreamGuardingError typhdr@(ftyp, header@FrameHeader{payloadLength}) = do
        settings <- liftIO $ readIORef http2settings
        case checkFrameHeader settings typhdr of
            Left h2err -> case h2err of
                StreamError err sid -> do
                    sendReset err sid
                    consume payloadLength
                    return True
                connErr -> liftIO $ IE.throwIO connErr
            Right _ -> do
                ex <- E.try $ controlOrStream ftyp header
                case ex of
                    Left (StreamError err sid) -> do
                        sendReset err sid
                        return True
                    Left connErr -> liftIO $ IE.throw connErr
                    Right cont -> return cont

    controlOrStream ftyp header@FrameHeader{streamId, payloadLength}
      | isControl streamId = do
          pl <- recvN payloadLength
          control ftyp header pl ctx
      | otherwise = do
          checkContinued
          !mstrm <- getStream
          pl <- recvN payloadLength
          case mstrm of
            Nothing -> do
                -- for h2spec only
                when (ftyp == FramePriority) $ do
                    PriorityFrame newpri <- guardIt $ decodePriorityFrame header pl
                    checkPriority newpri streamId
                return True -- just ignore this frame
            Just strm@Stream{streamState,streamPrecedence} -> do
              state <- liftIO $ readIORef streamState
              state' <- stream ftyp header pl ctx state strm
              case state' of
                  Open (NoBody tbl@(_,reqvt) pri) -> do
                      resetContinued
                      let mcl = readInt <$> getHeaderValue tokenContentLength reqvt
                      liftIO $ when (just mcl (/= (0 :: Int))) $
                          IE.throwIO $ StreamError ProtocolError streamId
                      liftIO $ writeIORef streamPrecedence $ toPrecedence pri
                      liftIO $ writeIORef streamState HalfClosed
                      (!req, !ii) <- mkreq tbl (Just 0, return "")
                      liftIO $ atomically $ writeTQueue inputQ $ Input strm req reqvt ii
                  Open (HasBody tbl@(_,reqvt) pri) -> do
                      resetContinued
                      q <- liftIO newTQueueIO
                      let !mcl = readInt <$> getHeaderValue tokenContentLength reqvt
                      liftIO $ writeIORef streamPrecedence $ toPrecedence pri
                      bodyLength <- liftIO $ newIORef 0
                      liftIO $ writeIORef streamState $ Open (Body q mcl bodyLength)
                      readQ <- newReadBody q
                      bodySource <- mkSource readQ
                      (!req, !ii) <- mkreq tbl (mcl, readSource bodySource)
                      liftIO $ atomically $ writeTQueue inputQ $ Input strm req reqvt ii
                  s@(Open Continued{}) -> do
                      setContinued
                      liftIO $ writeIORef streamState s
                  s -> do -- Idle, Open Body, HalfClosed, Closed
                      resetContinued
                      liftIO $ writeIORef streamState s
              return True
       where
         setContinued = liftIO $ writeIORef continued (Just streamId)
         resetContinued = liftIO $ writeIORef continued Nothing
         checkContinued = do
             mx <- liftIO $ readIORef continued
             case mx of
                 Nothing  -> return ()
                 Just sid
                   | sid == streamId && ftyp == FrameContinuation -> return ()
                   | otherwise -> liftIO $  IE.throwIO $ ConnectionError ProtocolError "continuation frame must follow"
         getStream = do
             mstrm0 <- search streamTable streamId
             case mstrm0 of
                 js@(Just strm0) -> do
                     when (ftyp == FrameHeaders) $ do
                         st <- liftIO $ readIORef $ streamState strm0
                         when (isHalfClosed st) $ liftIO $ IE.throwIO $ ConnectionError StreamClosed "header must not be sent to half closed"
                         -- Priority made an idele stream
                         when (isIdle st) $ opened ctx strm0
                     return js
                 Nothing
                   | isResponse streamId -> return Nothing
                   | otherwise           -> do
                         when (ftyp `notElem` [FrameHeaders,FramePriority]) $
                             liftIO $ IE.throwIO $ ConnectionError ProtocolError "this frame is not allowed in an idel stream"
                         csid <- liftIO $ readIORef clientStreamId
                         if streamId <= csid then
                             if ftyp == FramePriority then
                                 return Nothing -- will be ignored
                               else
                                 liftIO $ IE.throwIO $ ConnectionError ProtocolError "stream identifier must not decrease"
                           else do
                             liftIO $ when (ftyp == FrameHeaders) $ do
                                 writeIORef clientStreamId streamId
                                 cnt <- readIORef concurrency
                                 -- Checking the limitation of concurrency
                                 when (cnt >= maxConcurrency) $
                                     IE.throwIO $ StreamError RefusedStream streamId
                             ws <- liftIO $ initialWindowSize <$> readIORef http2settings
                             newstrm <- newStream streamId (fromIntegral ws)
                             when (ftyp == FrameHeaders) $ opened ctx newstrm
                             insert streamTable streamId newstrm
                             return $ Just newstrm

    consume = void . recvN

maxConcurrency :: Int
maxConcurrency = recommendedConcurrency

initialFrame :: ByteString
initialFrame = settingsFrame id [(SettingsMaxConcurrentStreams,maxConcurrency)]

----------------------------------------------------------------

control :: FrameTypeId -> FrameHeader -> ByteString -> Context -> Fiber Bool
control FrameSettings header@FrameHeader{flags} bs Context{http2settings, controlQ, firstSettings, streamTable} = do
    SettingsFrame alist <- guardIt $ decodeSettingsFrame header bs
    case checkSettingsList alist of
        Just x  -> liftIO $ IE.throwIO x
        Nothing -> return ()
    -- HTTP/2 Setting from a browser
    unless (testAck flags) $ do
        oldws <- liftIO $ initialWindowSize <$> readIORef http2settings
        liftIO $ modifyIORef' http2settings $ \old -> updateSettings old alist
        newws <- liftIO $ initialWindowSize <$> readIORef http2settings
        let diff = newws - oldws
        when (diff /= 0) $ updateAllStreamWindow (+ diff) streamTable
        let !frame = settingsFrame setAck []
        sent <- liftIO $ readIORef firstSettings
        let !setframe
              | sent      = CSettings               frame alist
              | otherwise = CSettings0 initialFrame frame alist
        liftIO $ unless sent $ writeIORef firstSettings True
        enqueueControl controlQ setframe
    return True

control FramePing FrameHeader{flags} bs Context{controlQ} =
    if testAck flags then
        return True -- just ignore
      else do
        let !frame = pingFrame bs
        enqueueControl controlQ $ CFrame frame
        return True

control FrameGoAway _ _ Context{controlQ} = do
    enqueueControl controlQ CFinish
    return False

control FrameWindowUpdate header bs Context{connectionWindow} = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    !w <- liftIO $ atomically $ do
      w0 <- readTVar connectionWindow
      let !w1 = w0 + n
      writeTVar connectionWindow w1
      return w1
    when (isWindowOverflow w) $ liftIO $ IE.throwIO $ ConnectionError FlowControlError "control window should be less than 2^31"
    return True

control _ _ _ _ =
    -- must not reach here
    return False

----------------------------------------------------------------

{-# INLINE guardIt #-}
guardIt :: Either HTTP2Error a -> Fiber a
guardIt x = case x of
    Left err    -> liftIO $ IE.throwIO err
    Right frame -> return frame


{-# INLINE checkPriority #-}
checkPriority :: Priority -> StreamId -> Fiber ()
checkPriority p me
  | dep == me = liftIO $ IE.throwIO $ StreamError ProtocolError me
  | otherwise = return ()
  where
    dep = streamDependency p

stream :: FrameTypeId -> FrameHeader -> ByteString -> Context -> StreamState -> Stream -> Fiber StreamState
stream FrameHeaders header@FrameHeader{flags} bs ctx (Open JustOpened) Stream{streamNumber} = do
    HeadersFrame mp frag <- guardIt $ decodeHeadersFrame header bs
    pri <- case mp of
        Nothing -> return defaultPriority
        Just p  -> do
            checkPriority p streamNumber
            return p
    let !endOfStream = testEndStream flags
        !endOfHeader = testEndHeader flags
    if endOfHeader then do
        tbl <- hpackDecodeHeader frag ctx -- fixme
        return $ if endOfStream then
                    Open (NoBody tbl pri)
                   else
                    Open (HasBody tbl pri)
      else do
        let !siz = BS.length frag
        return $ Open $ Continued [frag] siz 1 endOfStream pri

stream FrameHeaders header@FrameHeader{flags} bs _ (Open (Body q _ _)) _ = do
    -- trailer is not supported.
    -- let's read and ignore it.
    HeadersFrame _ _ <- guardIt $ decodeHeadersFrame header bs
    let !endOfStream = testEndStream flags
    if endOfStream then do
        liftIO $ atomically $ writeTQueue q ""
        return HalfClosed
      else
        -- we don't support continuation here.
        liftIO $ IE.throwIO $ ConnectionError ProtocolError "continuation in trailer is not supported"

stream FrameData
       header@FrameHeader{flags,payloadLength,streamId}
       bs
       Context{controlQ} s@(Open (Body q mcl bodyLength))
       Stream{streamNumber} = do
    DataFrame body <- guardIt $ decodeDataFrame header bs
    let !endOfStream = testEndStream flags
    len0 <- liftIO $ readIORef bodyLength
    let !len = len0 + payloadLength
    liftIO $ writeIORef bodyLength len
    when (payloadLength /= 0) $ do
        let !frame1 = windowUpdateFrame 0 payloadLength
            !frame2 = windowUpdateFrame streamNumber payloadLength
            !frame = frame1 `BS.append` frame2
        enqueueControl controlQ $ CFrame frame
    liftIO $ atomically $ writeTQueue q body
    if endOfStream then do
        case mcl of
            Nothing -> return ()
            Just cl -> liftIO $ when (cl /= len) $ IE.throwIO $ StreamError ProtocolError streamId
        liftIO $ atomically $ writeTQueue q ""
        return HalfClosed
      else
        return s

stream FrameContinuation FrameHeader{flags} frag ctx (Open (Continued rfrags siz n endOfStream pri)) _ = do
    let !endOfHeader = testEndHeader flags
        !rfrags' = frag : rfrags
        !siz' = siz + BS.length frag
        !n' = n + 1
    when (siz' > 51200) $ -- fixme: hard coding: 50K
      liftIO $ IE.throwIO $ ConnectionError EnhanceYourCalm "Header is too big"
    when (n' > 10) $ -- fixme: hard coding
      liftIO $ IE.throwIO $ ConnectionError EnhanceYourCalm "Header is too fragmented"
    if endOfHeader then do
        let !hdrblk = BS.concat $ reverse rfrags'
        tbl <- hpackDecodeHeader hdrblk ctx
        return $ if endOfStream then
                    Open (NoBody tbl pri)
                   else
                    Open (HasBody tbl pri)
      else
        return $ Open $ Continued rfrags' siz' n' endOfStream pri

stream FrameWindowUpdate header@FrameHeader{streamId} bs _ s Stream{streamWindow} = do
    WindowUpdateFrame n <- guardIt $ decodeWindowUpdateFrame header bs
    !w <- liftIO $ atomically $ do
      w0 <- readTVar streamWindow
      let !w1 = w0 + n
      writeTVar streamWindow w1
      return w1
    when (isWindowOverflow w) $
        liftIO $ IE.throwIO $ StreamError FlowControlError streamId
    return s

stream FrameRSTStream header bs ctx _ strm = do
    RSTStreamFrame e <- guardIt $ decoderstStreamFrame header bs
    let !cc = Reset e
    closed ctx strm cc
    return $ Closed cc -- will be written to streamState again

stream FramePriority header bs Context{outputQ,priorityTreeSize} s Stream{streamNumber,streamPrecedence} = do
    PriorityFrame newpri <- guardIt $ decodePriorityFrame header bs
    checkPriority newpri streamNumber
    oldpre <- liftIO $ readIORef streamPrecedence
    let !newpre = toPrecedence newpri
    liftIO $ writeIORef streamPrecedence newpre
    if isIdle s then do
        n <- liftIO $ atomicModifyIORef' priorityTreeSize (\x -> (x+1,x+1))
        -- fixme hard coding
        when (n >= 20) $ liftIO $ IE.throwIO $ ConnectionError EnhanceYourCalm "too many idle priority frames"
        liftIO $ prepare outputQ streamNumber newpri
      else do
        mx <- liftIO $ delete outputQ streamNumber oldpre
        case mx of
            Nothing  -> return ()
            Just out -> enqueueOutput outputQ out
    return s

-- this ordering is important
stream FrameContinuation _ _ _ _ _ = liftIO $ IE.throwIO $ ConnectionError ProtocolError "continue frame cannot come here"
stream _ _ _ _ (Open Continued{}) _ = liftIO $ IE.throwIO $ ConnectionError ProtocolError "an illegal frame follows header/continuation frames"
-- Ignore frames to streams we have just reset, per section 5.1.
stream _ _ _ _ st@(Closed (ResetByMe _)) _ = return st
stream FrameData FrameHeader{streamId} _ _ _ _ = liftIO $ IE.throwIO $ StreamError StreamClosed streamId
stream _ FrameHeader{streamId} _ _ _ _ = liftIO $ IE.throwIO $ StreamError ProtocolError streamId

----------------------------------------------------------------

{-# INLINE newReadBody #-}
newReadBody :: TQueue ByteString -> Fiber (Fiber ByteString)
newReadBody q = do
    ref <- liftIO $ newIORef False
    return $ readBody q ref

{-# INLINE readBody #-}
readBody :: TQueue ByteString -> IORef Bool -> Fiber ByteString
readBody q ref = do
    eof <- liftIO $ readIORef ref
    if eof then
        return ""
      else do
        bs <- liftIO $ atomically $ readTQueue q
        liftIO $ when (bs == "") $ writeIORef ref True
        return bs
