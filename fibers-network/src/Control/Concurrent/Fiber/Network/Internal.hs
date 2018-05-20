module Control.Concurrent.Fiber.Network.Internal
  where
import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.MVar
import System.Posix.Types (Channel)
import Network.Socket (SocketType(..), Family(..), ProtocolNumber(..), SocketStatus(..), SockAddr(..))
import qualified Network.Socket as NS
import Foreign.C.Error (eINTR, getErrno)
import Control.Exception.Base (evaluate)
import Foreign.C.Error (throwErrno)
import Data.Typeable
import Data.Word
import Java

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
data {-# CLASS "java.net.InetAddress" #-} InetAddress =
  IA (Object# InetAddress)
  deriving Class


-- io :: Fiber a -> IO a
-- io (Fiber a) = IO a

foreign import java unsafe "@static eta.network.Utils.getSockAddr" 
  getSockAddr':: Channel -> IO InetSocketAddress
foreign import java unsafe "getAddress" 
  sockInetAddress :: InetSocketAddress -> InetAddress
foreign import java unsafe "getPort"
  sockPort :: InetSocketAddress -> Int
foreign import java unsafe "@static eta.network.Utils.inetAddrInt"
  inetAddrInt :: InetAddress -> Word32


getSockAddr = liftIO . getSockAddr'

readMVar = undefined

withSockAddr = undefined

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
