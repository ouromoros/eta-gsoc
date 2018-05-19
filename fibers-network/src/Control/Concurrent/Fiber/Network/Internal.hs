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

data Socket
  = MkSocket
            Channel              -- File Descriptor
            Family
            SocketType
            ProtocolNumber       -- Protocol Number
            (MVar SocketStatus)  -- Status Flag
  deriving Typeable

-- io :: Fiber a -> IO a
-- io (Fiber a) = IO a

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
  sock <- getSockAddr ch
  let inet = inetAddrInt (sockInetAddress sock)
      port = sockPort sock
  return $ SockAddrInet (fromIntegral port) inet

data {-# CLASS "java.net.SocketAddress" #-} SocketAddress =
  SA (Object# SocketAddress)
  deriving Class

 -- Below are complex and need to be changed

bindPortGen :: SocketType -> Int -> HostPreference -> Fiber Socket
bindPortGen sockettype = bindPortGenEx (defaultSocketOptions sockettype) sockettype

bindPortGenEx :: [(Sock.SocketOption, Int)] -> SocketType -> Int -> HostPreference -> Fiber Socket
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
    addrs <- NS.getAddrInfo (Just hints) host port
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
          bracketOnError
          (NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr))
          NS.close
          (\sock -> do
              mapM_ (\(opt,v) -> NS.setSocketOption sock opt v) sockOpts
              NS.bind sock (NS.addrAddress addr)
              return sock
          )
    tryAddrs addrs'

close :: Socket -> Fiber ()
close (MkSocket s _ _ _ socketStatus) = do
 modifyMVar socketStatus $ \ status ->
   case status of
     ConvertedToHandle ->
         ioError (userError ("close: converted to a Handle, use hClose instead"))
     Closed ->
         return status
     _ -> closeFdWith closeFd s >> return Closed


socket :: Family         -- Family Name (usually AF_INET)
       -> SocketType     -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> Fiber Socket      -- Unconnected Socket
socket family stype protocol = do
    c_stype <- packSocketTypeOrThrow "socket" stype
    fd      <- c_socket (packFamily family) c_stype protocol
    setNonBlockIfNeeded fd
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
