module Control.Concurrent.Fiber.Network.Internal
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
  ,PortNumber
  ,HostAddress
  ,mkInetSocketAddress
  ,getByAddress

  ,readMVar
  ,fiber

  ,threadWaitAccept
  ,threadWaitConnect
  ,threadWaitRead
  ,threadWaitWrite

  ,withSocketsDo
  ,withSockAddr
  ,getSockAddr
  ,isBlocking
  ,c_setsockopt
  ,socket2FD
  ,newSockAddr
  ,packSocketOption
  ,packSocketTypeOrThrow
  ,setSocketOption
  ,isAcceptable

  ,toJByteArray
  ,throwErrno
  ,throwErrnoIfRetry
  ,throwErrnoIfMinus1Retry
  ,throwErrnoIfMinus1RetryMayBlock
  ,throwErrnoIfRetryMayBlock
  ,throwSocketErrorIfMinus1Retry
  ,throwSocketErrorWaitRead
  ,throwSocketErrorWaitWrite
    )
  where
import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import System.Posix.Types (Channel(..))
import Network.Socket (SocketType(..), Family(..), ProtocolNumber(..), SocketStatus(..), SockAddr(..), SocketOption(..), HostAddress, PortNumber)
import qualified Network.Socket as NS
import Foreign.C.Error (eINTR, getErrno, throwErrno, eWOULDBLOCK, eAGAIN)
import Control.Exception.Base (evaluate)
import Control.Monad
import Foreign.C.Types
import Data.Typeable
import Data.Word
import Data.Bits
import GHC.IO (IO(..))
import GHC.IO.FD (FD(..), FDType(..))
import GHC.Base
import Java
import Java.Core

data Socket
  = MkSocket
            Channel              -- File Descriptor
            Family
            SocketType
            ProtocolNumber       -- Protocol Number
            (MVar SocketStatus)  -- Status Flag
  deriving Typeable

data {-# CLASS "java.net.InetSocketAddress" #-} InetSocketAddress =
  ISA (Object# InetSocketAddress)
  deriving Class
type instance Inherits InetSocketAddress = '[SocketAddress]
data {-# CLASS "java.net.InetAddress" #-} InetAddress =
  IA (Object# InetAddress)
  deriving Class
type instance Inherits InetAddress = '[Object]


foreign import java unsafe "@new" mkInetSocketAddress
  :: InetAddress -> Int -> InetSocketAddress

foreign import java unsafe "@static java.net.InetAddress.getByAddress" getByAddress :: JByteArray -> InetAddress

fiber :: Fiber a -> IO a
fiber (Fiber a) = IO a

foreign import java unsafe "@static eta.network.Utils.getsockopt"
  c_getsockopt' :: Channel -> SOption -> IO CInt
foreign import java unsafe "@static eta.network.Utils.setsockopt"
  c_setsockopt' :: Channel -> SOption -> CInt ->  IO ()
foreign import java unsafe "@static eta.network.Utils.getSockAddr" 
  getSockAddr':: Channel -> IO InetSocketAddress
foreign import java unsafe "getAddress" 
  sockInetAddress :: InetSocketAddress -> InetAddress
foreign import java unsafe "getPort"
  sockPort :: InetSocketAddress -> Int
foreign import java unsafe "@static eta.network.Utils.inetAddrInt"
  inetAddrInt :: InetAddress -> Word32
foreign import java unsafe "@static eta.network.Utils.isBlocking"
  isBlocking' :: Channel -> IO Bool


registerRead :: Channel -> Fiber ()
registerRead (Channel o) = Fiber $ \s ->
  case registerRead# o s of
    s' -> (# s', () #)
registerWrite :: Channel -> Fiber ()
registerWrite (Channel o) = Fiber $ \s ->
  case registerWrite# o s of
    s' -> (# s', () #)
registerAccept :: Channel -> Fiber ()
registerAccept (Channel o) = Fiber $ \s ->
  case registerAccept# o s of
    s' -> (# s', () #)
registerConnect :: Channel -> Fiber ()
registerConnect (Channel o) = Fiber $ \s ->
  case registerConnect# o s of
    s' -> (# s', () #)

threadWaitRead :: Channel -> Fiber ()
threadWaitRead c = registerRead c >> block
threadWaitWrite :: Channel -> Fiber ()
threadWaitWrite c = registerWrite c >> block
threadWaitAccept :: Channel -> Fiber ()
threadWaitAccept c = registerAccept c >> block
threadWaitConnect :: Channel -> Fiber ()
threadWaitConnect c = registerConnect c >> block

foreign import prim "eta.fiber.network.Utils.registerAccept"
  registerAccept# :: Object# a -> State# s -> State# s
foreign import prim "eta.fiber.network.Utils.registerRead"
  registerRead# :: Object# a -> State# s -> State# s
foreign import prim "eta.fiber.network.Utils.registerWrite"
  registerWrite# :: Object# a -> State# s -> State# s
foreign import prim "eta.fiber.network.Utils.registerConnect"
  registerConnect# :: Object# a -> State# s -> State# s

-- foreign import prim "eta.fiber.network.Utils.waitAccept"
--   threadWaitAccept# :: Object# a -> State# s -> State# s
-- foreign import prim "eta.fiber.network.Utils.waitConnect"
--   threadWaitConnect# :: Object# a -> State# s -> State# s
-- foreign import prim "eta.fiber.network.Utils.waitRead"
--   threadWaitRead# :: Object# a -> State# s -> State# s
-- foreign import prim "eta.fiber.network.Utils.waitWrite"
--   threadWaitWrite# :: Object# a -> State# s -> State# s

-- threadWaitAccept :: Channel -> Fiber ()
-- threadWaitAccept (Channel o) = Fiber $ \s ->
--   case threadWaitAccept# o s of
--     s' -> (# s', () #)
-- threadWaitConnect :: Channel -> Fiber ()
-- threadWaitConnect (Channel o) = Fiber $ \s ->
--   case threadWaitConnect# o s of
--     s' -> (# s', () #)
-- threadWaitWrite :: Channel -> Fiber ()
-- threadWaitWrite (Channel o) = Fiber $ \s ->
--   case threadWaitWrite# o s of
--     s' -> (# s', () #)
-- threadWaitRead :: Channel -> Fiber ()
-- threadWaitRead (Channel o) = Fiber $ \s ->
--   case threadWaitRead# o s of
--     s' -> (# s', () #)

getSockAddr = liftIO . getSockAddr'
c_setsockopt c so i = liftIO $ c_setsockopt' c so i
c_getsockopt c so = liftIO $ c_getsockopt' c so
isBlocking = liftIO . isBlocking'

readMVar :: MVar a -> Fiber a
readMVar m = do
  a <- takeMVar m
  putMVar m a
  return a

toJByteArray :: [Word8] -> JByteArray
toJByteArray word8s = toJava bytes
  where bytes = map fromIntegral word8s :: [Byte]

withSockAddr :: SockAddr -> (SocketAddress -> Fiber a) -> Fiber a
withSockAddr addr f = case addr of
  SockAddrInet port host ->
    f $ superCast $ mkInetSocketAddress
      (getByAddress . toJByteArray $ padWord8s 4 host)
      (fromIntegral port)
  SockAddrInet6 port _ (w1, w2, w3, w4) _ ->
    f $ superCast $ mkInetSocketAddress
      (getByAddress . toJByteArray $ concatMap (padWord8s 4) [w1, w2, w3, w4])
      (fromIntegral port)
  _ -> error "Network.Socket.Types.withSockAddr: Invalid socket address type."
  where toWord8s :: (Bits a, Integral a) => a -> [Word8]
        toWord8s 0 = []
        toWord8s n = fromIntegral (n .&. 255) : toWord8s (n `shiftR` 8)

        padWord8s :: (Bits a, Integral a) => Int -> a -> [Word8]
        padWord8s n a = replicate (abs (n - length word8s)) 0 ++ word8s
          where word8s = reverse (toWord8s a)

socket2FD :: Socket -> Fiber FD
socket2FD  (MkSocket fd _ _ _ _) = do
  blocking <- isBlocking fd
  return $ FD { fdFD = FDGeneric fd, fdIsNonBlocking = not blocking }

isAcceptable :: Socket -> Fiber Bool
-- #if defined(DOMAIN_SOCKET_SUPPORT)
-- isAcceptable (MkSocket _ AF_UNIX x _ status)
--     | x == Stream || x == SeqPacket = do
--         value <- readMVar status
--         return (value == Connected || value == Bound || value == Listening)
-- isAcceptable (MkSocket _ AF_UNIX _ _ _) = return False
-- #endif
isAcceptable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected || value == Listening)


{-# INLINE withSocketsDo #-}
withSocketsDo :: Fiber a -> Fiber a
#if !defined(WITH_WINSOCK)
withSocketsDo x = x
#else
withSocketsDo act = (liftIO $ evaluate withSocketsInit) >> act


{-# NOINLINE withSocketsInit #-}
withSocketsInit :: ()
-- Use a CAF to make forcing it do initialisation once, but subsequent forces will be cheap
withSocketsInit = unsafePerformIO $ do
    x <- initWinSock
    when (x /= 0) $ ioError $
      userError "Network.Socket.Internal.withSocketsDo: Failed to initialise WinSock"

foreign import ccall unsafe "initWinSock" initWinSock :: IO Int

#endif

throwErrnoIfRetry            :: (a -> Bool) -> String -> Fiber a -> Fiber a
throwErrnoIfRetry pred loc f  =
  do
    res <- f
    if pred res
      then do
        err <- liftIO getErrno
        if err == eINTR
          then throwErrnoIfRetry pred loc f
          else liftIO (throwErrno loc)
      else return res

throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> Fiber a -> Fiber a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== -1)

throwErrnoIfMinus1RetryMayBlock :: (Eq a, Num a)
                                => String -> Fiber a -> Fiber b -> Fiber a
throwErrnoIfMinus1RetryMayBlock  = throwErrnoIfRetryMayBlock (== -1)

throwSocketErrorIfMinus1Retry
    :: (Eq a, Num a)
    => String  -- ^ textual description of the location
    -> Fiber a    -- ^ the 'IO' operation to be executed
    -> Fiber a

{-# SPECIALIZE throwSocketErrorIfMinus1Retry :: String -> Fiber CInt -> Fiber CInt #-}
throwSocketErrorIfMinus1Retry = throwErrnoIfMinus1Retry

throwSocketErrorWaitRead :: (Eq a, Num a) => Socket -> String -> Fiber a -> Fiber a
throwSocketErrorWaitRead _sock _name io = io

throwSocketErrorWaitWrite :: (Eq a, Num a) => Socket -> String -> Fiber a -> Fiber a
throwSocketErrorWaitWrite _sock _name io = io

-- is GetError useful for fiber?
throwErrnoIfRetryMayBlock
                :: (a -> Bool)  -- ^ predicate to apply to the result value
                                -- of the 'IO' operation
                -> String       -- ^ textual description of the location
                -> Fiber a         -- ^ the 'IO' operation to be executed
                -> Fiber b         -- ^ action to execute before retrying if
                                -- an immediate retry would block
                -> Fiber a
throwErrnoIfRetryMayBlock pred loc f on_block  =
  do
    res <- f
    if pred res
      then do
        err <- liftIO getErrno
        if err == eINTR
          then throwErrnoIfRetryMayBlock pred loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
                 then do _ <- on_block
                         throwErrnoIfRetryMayBlock pred loc f on_block
                 else liftIO $ throwErrno loc
      else return res

-- The below are copied from Network library since they are not accessible

newSockAddr :: Channel -> Fiber SockAddr
newSockAddr ch = do
  sock <- liftIO $ getSockAddr ch
  let inet = inetAddrInt (sockInetAddress sock)
      port = sockPort sock
  return $ SockAddrInet (fromIntegral port) inet

data {-# CLASS "java.net.SocketAddress" #-} SocketAddress =
  SA (Object# SocketAddress)
  deriving Class

packSocketOption :: SocketOption -> Maybe SOption
packSocketOption so =
  case Just so of
    Just ReuseAddr     -> Just sO_REUSEADDR
    Just Broadcast     -> Just sO_BROADCAST
    Just SendBuffer    -> Just sO_SNDBUF
    Just RecvBuffer    -> Just sO_RCVBUF
    Just KeepAlive     -> Just sO_KEEPALIVE
    Just Linger        -> Just sO_LINGER
    Just NoDelay       -> Just tCP_NODELAY
    _                  -> Nothing

packSocketOption' :: String -> SocketOption -> Fiber SOption
packSocketOption' caller so = liftIO $ maybe err return (packSocketOption so)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller,
    ": socket option ", show so, " unsupported on this system"]

packSocketType' :: SocketType -> Maybe CInt
packSocketType' stype = case Just stype of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just NoSocketType     -> Just 0
    Just Stream           -> Just 1
    Just Datagram         -> Just 2
    Just (ServerSocket _) -> Just 3
    _                     -> Nothing

packSocketTypeOrThrow :: String -> SocketType -> Fiber CInt
packSocketTypeOrThrow caller stype = liftIO $ maybe err return (packSocketType' stype)
 where
  err = ioError . userError . concat $ ["Network.Socket.", caller, ": ",
    "socket type ", show stype, " unsupported on this system"]


setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> Fiber ()
setSocketOption (MkSocket s _ _ _ _) so v = do
   opt <- packSocketOption' "setSocketOption" so
   c_setsockopt s opt (fromIntegral v)

getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> Fiber Int        -- Option Value
getSocketOption (MkSocket s _ _ _ _) so = do
   opt <- packSocketOption' "getSocketOption" so
   fmap fromIntegral $ c_getsockopt s opt


data {-# CLASS "java.net.SocketOption" #-} SOption = SOption (Object# SOption)

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.IP_MULTICAST_IF"
  iP_MULTICAST_IF :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.IP_MULTICAST_LOOP"
  iP_MULTICAST_LOOP :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.IP_MULTICAST_TTL"
  iP_MULTICAST_TTL :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.IP_TOS"
  iP_TOS :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.SO_BROADCAST"
  sO_BROADCAST :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.SO_KEEPALIVE"
  sO_KEEPALIVE :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.SO_LINGER"
  sO_LINGER :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.SO_RCVBUF"
  sO_RCVBUF :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.SO_REUSEADDR"
  sO_REUSEADDR :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.SO_SNDBUF"
  sO_SNDBUF :: SOption

foreign import java unsafe
  "@static @field java.net.StandardSocketOptions.TCP_NODELAY"
  tCP_NODELAY :: SOption

