{-|

This module is from `Network.Wai` and defines some utility functions regarding `Response`.

-}
module Network.Wai.Handler.Warp.ResponseBuilder
    (
      -- * Types
      Application
    , ResponseReceived
      -- * Response
    , Response
    , StreamingBody
    , FilePart (..)
      -- ** Response composers
    , responseFile
    , responseBuilder
    , responseLBS
    , responseRaw
      -- ** Response accessors
    , responseStatus
    , responseHeaders
    ) where

import           Data.ByteString.Builder      (Builder, lazyByteString)
import           Data.ByteString.Builder      (byteString)
import           Control.Monad                (unless)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Lazy.Internal as LI
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteString.Lazy.Char8   ()
import           Data.Function                (fix)
import qualified Network.HTTP.Types           as H
import           Control.Concurrent.Fiber.Network (SockAddr (SockAddrInet))
import           Network.Wai.Internal         hiding (Request(..), Application, StreamingBody, Response(..), responseLBS, responseBuilder)
import           Network.Wai.Handler.Warp.Types
import           Control.Concurrent.Fiber
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafeInterleaveIO)

----------------------------------------------------------------

-- | Creating 'Response' from a file.
responseFile :: H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response
responseFile = ResponseFile

-- | Creating 'Response' from 'Builder'.
--
-- Some questions and answers about the usage of 'Builder' here:
--
-- Q1. Shouldn't it be at the user's discretion to use Builders internally and
-- then create a stream of ByteStrings?
--
-- A1. That would be less efficient, as we wouldn't get cheap concatenation
-- with the response headers.
--
-- Q2. Isn't it really inefficient to convert from ByteString to Builder, and
-- then right back to ByteString?
--
-- A2. No. If the ByteStrings are small, then they will be copied into a larger
-- buffer, which should be a performance gain overall (less system calls). If
-- they are already large, then an insert operation is used
-- to avoid copying.
--
-- Q3. Doesn't this prevent us from creating comet-style servers, since data
-- will be cached?
--
-- A3. You can force a Builder to output a ByteString before it is an
-- optimal size by sending a flush command.
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> Response
responseBuilder = ResponseBuilder

-- | Creating 'Response' from 'L.ByteString'. This is a wrapper for
--   'responseBuilder'.
responseLBS :: H.Status -> H.ResponseHeaders -> L.ByteString -> Response
responseLBS s h = ResponseBuilder s h . lazyByteString

-- | Creating 'Response' from a stream of values.
--
-- In order to allocate resources in an exception-safe manner, you can use the
-- @bracket@ pattern outside of the call to @responseStream@. As a trivial
-- example:
--
-- @
-- app :: Application
-- app req respond = bracket_
--     (putStrLn \"Allocating scarce resource\")
--     (putStrLn \"Cleaning up\")
--     $ respond $ responseStream status200 [] $ \\write flush -> do
--         write $ byteString \"Hello\\n\"
--         flush
--         write $ byteString \"World\\n\"
-- @
--
-- Note that in some cases you can use @bracket@ from inside @responseStream@
-- as well. However, placing the call on the outside allows your status value
-- and response headers to depend on the scarce resource.
--
-- Since 3.0.0
responseStream :: H.Status
               -> H.ResponseHeaders
               -> StreamingBody
               -> Response
responseStream = ResponseStream

-- | Create a response for a raw application. This is useful for \"upgrade\"
-- situations such as WebSockets, where an application requests for the server
-- to grant it raw network access.
--
-- This function requires a backup response to be provided, for the case where
-- the handler in question does not support such upgrading (e.g., CGI apps).
--
-- In the event that you read from the request body before returning a
-- @responseRaw@, behavior is undefined.
--
-- Since 2.1.0
responseRaw :: (Fiber B.ByteString -> (B.ByteString -> Fiber ()) -> Fiber ())
            -> Response
            -> Response
responseRaw = ResponseRaw

----------------------------------------------------------------

-- | Accessing 'H.Status' in 'Response'.
responseStatus :: Response -> H.Status
responseStatus (ResponseFile    s _ _ _) = s
responseStatus (ResponseBuilder s _ _  ) = s
responseStatus (ResponseStream  s _ _  ) = s
responseStatus (ResponseRaw _ res      ) = responseStatus res

-- | Accessing 'H.ResponseHeaders' in 'Response'.
responseHeaders :: Response -> H.ResponseHeaders
responseHeaders (ResponseFile    _ hs _ _) = hs
responseHeaders (ResponseBuilder _ hs _  ) = hs
responseHeaders (ResponseStream  _ hs _  ) = hs
responseHeaders (ResponseRaw _ res)        = responseHeaders res


-- | Apply the provided function to the response header list of the Response.
mapResponseHeaders :: (H.ResponseHeaders -> H.ResponseHeaders) -> Response -> Response
mapResponseHeaders f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapResponseHeaders f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapResponseHeaders f (ResponseStream s h b) = ResponseStream s (f h) b
mapResponseHeaders _ r@(ResponseRaw _ _) = r

-- | Apply the provided function to the response status of the Response.
mapResponseStatus :: (H.Status -> H.Status) -> Response -> Response
mapResponseStatus f (ResponseFile s h b1 b2) = ResponseFile (f s) h b1 b2
mapResponseStatus f (ResponseBuilder s h b) = ResponseBuilder (f s) h b
mapResponseStatus f (ResponseStream s h b) = ResponseStream (f s) h b
mapResponseStatus _ r@(ResponseRaw _ _) = r

----------------------------------------------------------------


-- | A default, blank request.
--
-- Since 2.0.0
defaultRequest :: Request
defaultRequest = Request
    { requestMethod = H.methodGet
    , httpVersion = H.http10
    , rawPathInfo = B.empty
    , rawQueryString = B.empty
    , requestHeaders = []
    , isSecure = False
    , remoteHost = SockAddrInet 0 0
    , pathInfo = []
    , queryString = []
    , requestBody = return B.empty
    , vault = mempty
    , requestBodyLength = KnownLength 0
    , requestHeaderHost = Nothing
    , requestHeaderRange = Nothing
    , requestHeaderReferer = Nothing
    , requestHeaderUserAgent = Nothing
    }
