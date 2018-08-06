{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import Control.Concurrent.Fiber.Exception
import Control.Exception (Exception)
import qualified Data.ByteString as S
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)
import System.Posix.Types (Fd)

import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.Imports
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai hiding (Application, Request, Response(..), StreamingBody)
import qualified Network.HTTP.Types as H
import Control.Concurrent.Fiber.Network (SockAddr(..))
import Data.Text (Text)
import Data.Vault.Lazy (Vault)

import           Data.ByteString.Builder      (Builder)

----------------------------------------------------------------

-- | TCP port number.
type Port = Int

type Application = Request -> (Response -> Fiber ResponseReceived) -> Fiber ResponseReceived

----------------------------------------------------------------

-- | The type for header value used with 'HeaderName'.
type HeaderValue = ByteString

----------------------------------------------------------------

-- | Error types for bad 'Request'.
data InvalidRequest = NotEnoughLines [String]
                    | BadFirstLine String
                    | NonHttp
                    | IncompleteHeaders
                    | ConnectionClosedByPeer
                    | OverLargeHeader
                    | BadProxyHeader String
                    deriving (Eq, Typeable)

instance Show InvalidRequest where
    show (NotEnoughLines xs) = "Warp: Incomplete request headers, received: " ++ show xs
    show (BadFirstLine s) = "Warp: Invalid first line of request: " ++ show s
    show NonHttp = "Warp: Request line specified a non-HTTP request"
    show IncompleteHeaders = "Warp: Request headers did not finish transmission"
    show ConnectionClosedByPeer = "Warp: Client closed connection prematurely"
    show OverLargeHeader = "Warp: Request headers too large, possible memory attack detected. Closing connection."
    show (BadProxyHeader s) = "Warp: Invalid PROXY protocol header: " ++ show s

instance Exception InvalidRequest

----------------------------------------------------------------

-- | Data type to abstract file identifiers.
--   On Unix, a file descriptor would be specified to make use of
--   the file descriptor cache.
--
-- Since: 3.1.0
data FileId = FileId {
    fileIdPath :: FilePath
  , fileIdFd   :: Maybe Fd
  }

-- |  fileid, offset, length, hook action, HTTP headers
--
-- Since: 3.1.0
type SendFile = FileId -> Integer -> Integer -> Fiber () -> [ByteString] -> Fiber ()

-- | Type for read buffer pool
type BufferPool = IORef ByteString

-- | Type for buffer
type Buffer = Ptr Word8

-- | Type for buffer size
type BufSize = Int

-- | Type for the action to receive input data
type Recv = Fiber ByteString

-- | Type for the action to receive input data with a buffer.
--   The result boolean indicates whether or not the buffer is fully filled.
type RecvBuf = Buffer -> BufSize -> Fiber Bool

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection {
    -- | This is not used at this moment.
      connSendMany    :: [ByteString] -> Fiber ()
    -- | The sending function.
    , connSendAll     :: ByteString -> Fiber ()
    -- | The sending function for files in HTTP/1.1.
    , connSendFile    :: SendFile
    -- | The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    , connClose       :: IO ()
    -- | Free any buffers allocated. Warp guarantees it will only be
    -- called once, and no other functions will be called after it.
    , connFree        :: Fiber ()
    -- | The connection receiving function. This returns "" for EOF.
    , connRecv        :: Recv
    -- | The connection receiving function. This tries to fill the buffer.
    --   This returns when the buffer is filled or reaches EOF.
    , connRecvBuf     :: RecvBuf
    -- | The write buffer.
    , connWriteBuffer :: Buffer
    -- | The size of the write buffer.
    , connBufferSize  :: BufSize
    }

----------------------------------------------------------------

type Hash = Int


data InternalInfo0 =
    InternalInfo0 T.Manager
                  (Fiber D.GMTDate)
                  (Hash -> FilePath -> Fiber (Maybe F.Fd, F.Refresh))
                  (Hash -> FilePath -> Fiber I.FileInfo)

timeoutManager0 :: InternalInfo0 -> T.Manager
timeoutManager0 (InternalInfo0 tm _ _ _) = tm

data InternalInfo1 =
    InternalInfo1 T.Handle
                  T.Manager
                  (Fiber D.GMTDate)
                  (Hash -> FilePath -> Fiber (Maybe F.Fd, F.Refresh))
                  (Hash -> FilePath -> Fiber I.FileInfo)

toInternalInfo1 :: InternalInfo0 -> T.Handle -> InternalInfo1
toInternalInfo1 (InternalInfo0 b c d e) a = InternalInfo1 a b c d e

threadHandle1 :: InternalInfo1 -> T.Handle
threadHandle1 (InternalInfo1 th _ _ _ _) = th

data InternalInfo = InternalInfo {
    threadHandle   :: T.Handle
  , timeoutManager :: T.Manager
  , getDate        :: Fiber D.GMTDate
  , getFd          :: FilePath -> Fiber (Maybe F.Fd, F.Refresh)
  , getFileInfo    :: FilePath -> Fiber I.FileInfo
  }

toInternalInfo :: InternalInfo1 -> Hash -> InternalInfo
toInternalInfo (InternalInfo1 a b c d e) h = InternalInfo a b c (d h) (e h)

----------------------------------------------------------------

-- | Type for input streaming.
data Source = Source !(IORef ByteString) !(Fiber ByteString)

mkSource :: Fiber ByteString -> Fiber Source
mkSource func = do
    ref <- liftIO $ newIORef S.empty
    return $! Source ref func

readSource :: Source -> Fiber ByteString
readSource (Source ref func) = do
    bs <- liftIO $ readIORef ref
    if S.null bs
        then func
        else do
            liftIO $ writeIORef ref S.empty
            return bs

-- | Read from a Source, ignoring any leftovers.
readSource' :: Source -> Fiber ByteString
readSource' (Source _ func) = func

leftoverSource :: Source -> ByteString -> Fiber ()
leftoverSource (Source ref _) bs = liftIO $ writeIORef ref bs

readLeftoverSource :: Source -> Fiber ByteString
readLeftoverSource (Source ref _) = liftIO $ readIORef ref

----------------------------------------------------------------

-- | What kind of transport is used for this connection?
data Transport = TCP -- ^ Plain channel: TCP
               | TLS {
                   tlsMajorVersion :: Int
                 , tlsMinorVersion :: Int
                 , tlsNegotiatedProtocol :: Maybe ByteString -- ^ The result of Application Layer Protocol Negociation in RFC 7301
                 , tlsChiperID :: Word16
                 }  -- ^ Encrypted channel: TLS or SSL

isTransportSecure :: Transport -> Bool
isTransportSecure TCP = False
isTransportSecure _   = True

data Request = Request {
     requestMethod        :: H.Method
  ,  httpVersion          :: H.HttpVersion
  ,  rawPathInfo          :: S.ByteString
  ,  rawQueryString       :: S.ByteString
  ,  requestHeaders       :: H.RequestHeaders
  ,  isSecure             :: Bool
  ,  remoteHost           :: SockAddr
  ,  pathInfo             :: [Text]
  ,  queryString          :: H.Query
  ,  requestBody          :: Fiber S.ByteString
  ,  vault                 :: Vault
  ,  requestBodyLength     :: RequestBodyLength
  ,  requestHeaderHost     :: Maybe S.ByteString
  ,  requestHeaderRange   :: Maybe S.ByteString
  ,  requestHeaderReferer   :: Maybe S.ByteString
  ,  requestHeaderUserAgent :: Maybe S.ByteString
  }
  deriving (Typeable)

instance Show Request where
    show Request{..} = "Request {" ++ intercalate ", " [a ++ " = " ++ b | (a,b) <- fields] ++ "}"
        where
            fields =
                [("requestMethod",show requestMethod)
                ,("httpVersion",show httpVersion)
                ,("rawPathInfo",show rawPathInfo)
                ,("rawQueryString",show rawQueryString)
                ,("requestHeaders",show requestHeaders)
                ,("isSecure",show isSecure)
                ,("remoteHost",show remoteHost)
                ,("pathInfo",show pathInfo)
                ,("queryString",show queryString)
                ,("requestBody","<IO ByteString>")
                ,("vault","<Vault>")
                ,("requestBodyLength",show requestBodyLength)
                ,("requestHeaderHost",show requestHeaderHost)
                ,("requestHeaderRange",show requestHeaderRange)
                ]

defaultRequest :: Request
defaultRequest = Request
    { requestMethod = H.methodGet
    , httpVersion = H.http10
    , rawPathInfo = S.empty
    , rawQueryString = S.empty
    , requestHeaders = []
    , isSecure = False
    , remoteHost = SockAddrInet 0 0
    , pathInfo = []
    , queryString = []
    , requestBody = return S.empty
    , vault = mempty
    , requestBodyLength = KnownLength 0
    , requestHeaderHost = Nothing
    , requestHeaderRange = Nothing
    , requestHeaderReferer = Nothing
    , requestHeaderUserAgent = Nothing
    }

data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseStream H.Status H.ResponseHeaders StreamingBody
    | ResponseRaw (Fiber S.ByteString -> (S.ByteString -> Fiber ()) -> Fiber ()) Response
  deriving Typeable

-- | Represents a streaming HTTP response body. It's a function of two
-- parameters; the first parameter provides a means of sending another chunk of
-- data, and the second parameter provides a means of flushing the data to the
-- client.
--
-- Since 3.0.0
type StreamingBody = (Builder -> Fiber ()) -> Fiber () -> Fiber ()
