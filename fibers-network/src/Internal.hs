Module Control.Concurrent.Fiber.Network.Internal
  where
import Control.Concurrent.FIber
import System.Posix.Types (Channel)
import Network.Types (SocketType(..), Family(..), ProtocolNumber(..), SocketStatus(..), SocketAddr(..), SocketAddress)
import Foreign.C.Error (eINTR, getErrno)
import Control.Exception.Base (evaluate)

data Socket
  = MkSocket
            Channel              -- File Descriptor
            Family
            SocketType
            ProtocolNumber       -- Protocol Number
            (MVar SocketStatus)  -- Status Flag
  deriving Typeable

withSockAddr :: SockAddr -> (SocketAddress -> Fiber a) -> Fiber a

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
          else throwErrno loc
      else return res

throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> Fiber a -> Fiber a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== -1)
