Module Control.Concurrent.Fiber.Network.Internal
  where
import Control.Concurrent.FIber

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
