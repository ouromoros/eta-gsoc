{-# LANGUAGE BangPatterns, MultiWayIf, ScopedTypeVariables #-}
module Control.Concurrent.Fiber.Network
  (SocketType(..)
  ,Family(..)
  ,ProtocolNumber(..)
  ,SocketStatus(..)
  ,SockAddr(..)
  ,SocketOption(..)
  ,Socket(..)
  ,InetSocketAddress
  ,InetAddress
  ,SocketAddress
  ,AddrInfo(..)
  ,AddrInfoFlag(..)
  ,NameInfoFlag(..)
  ,mkInetSocketAddress
  ,getByAddress
  ,defaultSocketOptions
  ,defaultHints
  ,defaultProtocol
  ,getAddrInfo

  ,withSocketsDo
  ,withSockAddr
  ,getSockAddr
  ,isBlocking
  ,socket2FD
  ,newSockAddr
  ,isAcceptable
  ,setSocketOption
  ,setNonBlock

  ,accept
  ,connect
  ,listen
  ,send
  ,sendAll
  ,sendBuf
  ,sendMany
  ,recv
  ,close
  ,socket
  ,bind

  ,readRawBufferPtr
  ,writeRawBufferPtr
  ,bindPortTCP
  ,bindPortGen
  ,bindPortGenEx
    )
  where

import Control.Concurrent.Fiber.Network.Internal

import Control.Concurrent.Fiber (Fiber(..), liftIO, takeMVar, putMVar)
import qualified Control.Concurrent.Fiber as F
import qualified Control.Concurrent.MVar as M

import qualified Network.Socket as NS
import qualified Data.ByteString as B

import GHC.IO.FD (FD(..), FDType(..), fdChannel)
import Foreign
import Foreign.C.String (withCStringLen)
import Foreign.C.Types
import Java
import System.Posix.Types (Channel)
import System.Posix.Internals (c_close)
import qualified System.Posix.Internals as SPI
import qualified Data.Streaming.Network as DSN

import Data.Streaming.Network (HostPreference(..))
import Data.Streaming.Network.Internal (HostPreference(..))
import Network.Socket (AddrInfo(..), SocketType(..), Family(..), ProtocolNumber(..), SocketStatus(..), SockAddr(..), SocketOption(..), AddrInfoFlag(..), NameInfoFlag(..), defaultProtocol)

import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Control.Monad
import Control.Exception (throwIO, catch, IOException, bracketOnError)
import System.IO.Error 
import GHC.Conc (closeFdWith) -- blocking?
import GHC.IO.Exception
-- import Data.Streaming.Network (HostPreference)

foreign import java unsafe "@static eta.network.Utils.connect"
  c_connect' :: Channel -> SocketAddress -> IO Bool
foreign import java unsafe "@static eta.network.Utils.accept"
  c_accept' :: Channel -> IO (Maybe Channel)
foreign import java unsafe "@static eta.network.Utils.listen"
  c_listen' :: Channel -> SocketAddress -> CInt -> IO CInt
foreign import java unsafe "@static eta.network.Utils.bind"
  c_bind' :: Channel -> SocketAddress -> IO CInt
foreign import java unsafe "@static eta.network.Utils.socket"
  c_socket' :: CInt -> CInt -> CInt -> IO Channel

foreign import java unsafe "@static eta.runtime.concurrent.Concurrent.waitAccept"
  threadWaitAccept' :: Channel -> IO ()
foreign import java unsafe "@static eta.runtime.concurrent.Concurrent.waitConnect"
  threadWaitConnect' :: Channel -> IO ()
foreign import java unsafe "@static eta.runtime.concurrent.Concurrent.waitRead"
  threadWaitRead' :: Channel -> IO ()
foreign import java unsafe "@static eta.runtime.concurrent.Concurrent.waitWrite"
  threadWaitWrite' :: Channel -> IO ()
foreign import java unsafe "@static eta.fiber.network.Utils.setNonBlock"
  setNonBlock' :: Channel -> IO ()

setNonBlock :: Channel -> Fiber ()
setNonBlock = liftIO . setNonBlock'
threadWaitAccept :: Channel -> Fiber ()
threadWaitAccept = liftIO . threadWaitAccept'
threadWaitConnect :: Channel -> Fiber ()
threadWaitConnect = liftIO . threadWaitConnect'
threadWaitWrite :: Channel -> Fiber ()
threadWaitWrite = liftIO . threadWaitWrite'
threadWaitRead :: Channel -> Fiber ()
threadWaitRead = liftIO . threadWaitRead'

c_connect c sa = liftIO $ c_connect' c sa
c_accept = liftIO . c_accept'
c_read fd buf len = liftIO $ SPI.c_read fd buf len
c_write fd buf len = liftIO $ SPI.c_write fd buf len
c_listen c sa i = liftIO $ c_listen' c sa i
c_bind c sa = liftIO $ c_bind' c sa
c_socket family t p = liftIO $ c_socket' family t p

newMVar = liftIO . F.newMVar
modifyMVar_ m f = liftIO $ M.modifyMVar_ m (\x -> fiber (f x))
getAddrInfo mHints node service = liftIO $ NS.getAddrInfo mHints node service


accept :: Socket                        -- Queue Socket
       -> Fiber (Socket,                   -- Readable Socket
              SockAddr)                 -- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readMVar status
 okay <- isAcceptable sock
 if not okay
   then
     liftIO $ ioError $ userError $
       "Network.Socket.accept: can't accept socket (" ++
         show (family, stype, protocol) ++ ") with status not in position to accept" ++
         "connections."
   else do
     new_sock <- onNothingRetry $ c_accept s
     addr <- newSockAddr new_sock
     setNonBlock new_sock
     new_status <- newMVar Connected
     return ((MkSocket new_sock family (toStype stype) protocol new_status), addr)
  where onNothingRetry io = do
          mResult <- io
          case mResult of
            Nothing -> threadWaitAccept s >> onNothingRetry io
            Just ch -> return ch
        toStype (ServerSocket s) = s


connect :: Socket    -- Unconnected Socket
        -> SockAddr  -- Socket address stuff
        -> Fiber ()
connect sock@(MkSocket s _family _stype _protocol socketStatus) addr = withSocketsDo $ do
 modifyMVar_ socketStatus $ \currentStatus -> do
   if shouldError currentStatus
    then
      liftIO $ ioError $ userError $
        errLoc ++ ": can't connect to socket with status that is not bound or default."
    else do
      withSockAddr addr $ \saddr -> do

        let connectLoop = do
               connected <- c_connect s saddr
               unless connected $ do
                 connectBlocked
                 connectLoop
            connectBlocked = threadWaitConnect s

        connectLoop
        return Connected
 where
   -- TODO: add sock as Show instance
   errLoc = "Network.Socket.connect: " -- ++ show sock
   shouldError NotConnected = False
   shouldError (Bound _) = False
   shouldError _ = True

listen :: Socket  -- Connected & Bound Socket
       -> Int     -- Queue Length
       -> Fiber ()
listen (MkSocket s _family _stype _protocol socketStatus) backlog = do
 modifyMVar_ socketStatus $ \ status -> do
   if | Bound sockAddr <- status -> do
        withSockAddr sockAddr $ \saddr -> c_listen s saddr (fromIntegral backlog)
        return Listening
      | otherwise ->
        liftIO $ ioError $ userError $
          "Network.Socket.listen: can't listen on socket with non-bound status."


#if !defined(mingw32_HOST_OS)

readRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> Fiber Int
readRawBufferPtr loc !fd !buf !off !len = unsafe_read
  where
    do_read call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock loc call
                            (threadWaitRead (fdChannel fd))
    unsafe_read  = do_read (c_read (fdChannel fd) (buf `plusPtr` off) len)

writeRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> Fiber CInt
writeRawBufferPtr loc !fd !buf !off !len = unsafe_write
  where
    do_write call = fromIntegral `fmap`
                      throwErrnoIfMinus1RetryMayBlock loc call
                        (threadWaitWrite (fdChannel fd))
    unsafe_write  = do_write (c_write (fdChannel fd) (buf `plusPtr` off) len)

#else
  {- Implement for windows -}
#endif

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"
mkEOFError :: String -> IOError
mkEOFError loc = ioeSetErrorString (mkIOError EOF loc Nothing Nothing) "end of file"


recv :: Socket         -- ^ Connected socket
     -> Int            -- ^ Maximum number of bytes to receive
     -> Fiber ByteString  -- ^ Data received
recv sock nbytes
    | nbytes < 0 = liftIO $ ioError (mkInvalidRecvArgError "Network.Socket.ByteString.recv")
    | otherwise  = liftIO $ createAndTrim nbytes $ \ptr ->
        catch
          (fiber (recvBuf sock ptr nbytes))
          (\e -> if isEOFError e then return 0 else throwIO e)

recvBuf :: Socket -> Ptr Word8 -> Int -> Fiber Int
recvBuf sock@(MkSocket s _family _stype _protocol _status) ptr nbytes
 | nbytes <= 0 = liftIO $ ioError (mkInvalidRecvArgError "Network.Socket.recvBuf")
 | otherwise   = do
        fd <- socket2FD sock
        len <-
-- see comment in sendBuf above.
            throwSocketErrorIfMinus1Retry "Network.Socket.recvBuf" $
                readRawBufferPtr "Network.Socket.recvBuf"
                fd ptr 0 (fromIntegral nbytes)
        let len' = fromIntegral len
        if len' == 0
         then liftIO $ ioError (mkEOFError "Network.Socket.recvBuf")
         else return len'

sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> Fiber ()
sendAll sock bs = do
    sent <- send sock bs
    when (sent < B.length bs) $ sendAll sock (B.drop sent bs)

sendMany :: Socket        -- ^ Connected socket
         -> [ByteString]  -- ^ Data to send
         -> Fiber ()
sendMany sock = sendAll sock . B.concat

sendBuf :: Socket     -- Bound/Connected Socket
        -> Ptr Word8  -- Pointer to the data to send
        -> Int        -- Length of the buffer
        -> Fiber Int     -- Number of Bytes sent
sendBuf sock@(MkSocket s _family _stype _protocol _status) str len = do
   fd <- socket2FD sock
   liftM fromIntegral $
-- writeRawBufferPtr is supposed to handle checking for errors, but it's broken
-- on x86_64 because of GHC bug #12010 so we duplicate the check here. The call
-- to throwSocketErrorIfMinus1Retry can be removed when no GHC version with the
-- bug is supported.
    throwSocketErrorIfMinus1Retry "Network.Socket.sendBuf" $ writeRawBufferPtr
      "Network.Socket.sendBuf"
      fd
      (castPtr str)
      0
      (fromIntegral len)

send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> Fiber Int      -- ^ Number of bytes sent
send sock xs = liftIO $ unsafeUseAsCStringLen xs $ \(str, len) ->
  fiber $ sendBuf sock (castPtr str) len

closeFd = c_close

close :: Socket -> Fiber ()
close (MkSocket s _ _ _ socketStatus) = do
 modifyMVar_ socketStatus $ \ status ->
   case status of
     ConvertedToHandle ->
         liftIO $ ioError (userError ("close: converted to a Handle, use hClose instead"))
     Closed ->
         return status
     _ -> liftIO (closeFdWith closeFd s) >> return Closed

socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> Fiber Socket      -- Unconnected Socket
socket family stype protocol = do
    c_stype <- packSocketTypeOrThrow "socket" stype
    fd      <- c_socket (NS.packFamily family) c_stype protocol
    setNonBlock fd
    socket_status <- newMVar NotConnected
    withSocketsDo $ return ()
    let sock = MkSocket fd family stype protocol socket_status
#if HAVE_DECL_IPV6_V6ONLY
    -- The default value of the IPv6Only option is platform specific,
    -- so we explicitly set it to 0 to provide a common default.
# if defined(mingw32_HOST_OS)
    -- The IPv6Only option is only supported on Windows Vista and later,
    -- so trying to change it might throw an error.
    when (family == AF_INET6 && (stype == Stream || stype == Datagram)) $
      catch (setSocketOption sock IPv6Only 0) $ (\(_ :: IOException) -> return ())
# else
    when (family == AF_INET6 && (stype == Stream || stype == Datagram)) $
      setSocketOption sock IPv6Only 0 `onException` close sock
# endif
#endif
    return sock

bind :: Socket    -- Unconnected Socket
           -> SockAddr  -- Address to Bind to
           -> Fiber ()
bind (MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \ status -> do
  if status /= NotConnected
    then
     liftIO $ ioError $ userError $
       "Network.Socket.bind: can't bind to socket with non-default status."
    else do
     withSockAddr addr $ \saddr -> do
       _status <- c_bind s saddr
       return (Bound addr)

defaultHints :: AddrInfo
defaultHints = AddrInfo {
                         addrFlags = [],
                         addrFamily = AF_UNSPEC,
                         addrSocketType = NoSocketType,
                         addrProtocol = defaultProtocol,
                         addrAddress = undefined,
                         addrCanonName = undefined
                        }

-- Below are functions in Data.Streaming

defaultSocketOptions :: SocketType -> [(NS.SocketOption, Int)]
defaultSocketOptions sockettype =
    case sockettype of
        NS.Datagram -> [(NS.ReuseAddr,1)]
        _           -> [(NS.NoDelay,1), (NS.ReuseAddr,1)]

bindPortTCP :: Int -> HostPreference -> Fiber Socket
bindPortTCP p s = do
    sock <- bindPortGen Stream p s
    listen sock (max 2048 NS.maxListenQueue)
    return sock

bindPortGen :: SocketType -> Int -> HostPreference -> Fiber Socket
bindPortGen sockettype = bindPortGenEx (defaultSocketOptions sockettype) sockettype

bindPortGenEx :: [(NS.SocketOption, Int)] -> SocketType -> Int -> HostPreference -> Fiber Socket
bindPortGenEx sockOpts sockettype p s = do
    let hints = NS.defaultHints
            { NS.addrFlags = [ NS.AI_PASSIVE
                             , NS.AI_ADDRCONFIG
                             ]
            , NS.addrSocketType = sockettype
            }
        host =
            case s of
                Host s' -> Just s'
                _ -> Nothing
        port = Just . show $ p
    addrs <- liftIO $ getAddrInfo (Just hints) host port
    -- Choose an IPv6 socket if exists.  This ensures the socket can
    -- handle both IPv4 and IPv6 if v6only is false.
    let addrs4 = filter (\x -> NS.addrFamily x /= NS.AF_INET6) addrs
        addrs6 = filter (\x -> NS.addrFamily x == NS.AF_INET6) addrs
        addrs' =
            case s of
                HostIPv4     -> addrs4 ++ addrs6
                HostIPv4Only -> addrs4
                HostIPv6     -> addrs6 ++ addrs4
                HostIPv6Only -> addrs6
                _ -> addrs

        tryAddrs (addr1:rest@(_:_)) =
                                      catch
                                      (theBody addr1)
                                      (\(_ :: IOException) -> tryAddrs rest)
        tryAddrs (addr1:[])         = theBody addr1
        tryAddrs _                  = error "bindPort: addrs is empty"

        theBody addr =
          -- Is this Ok?
          bracketOnError
          (fiber (socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)))
          (fiber . close)
          (\sock -> do
              mapM_ (\(opt,v) -> fiber $ setSocketOption sock opt v) sockOpts
              --------
              fiber $ bind sock (NS.addrAddress addr)
              return sock
          )
    liftIO $ tryAddrs addrs'

