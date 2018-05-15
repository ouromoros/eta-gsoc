Module Control.Concurrent.Fiber.Network
  where
import Control.Concurrent.Fiber
import qualified Network.Socket as Sock
import qualified Network.Socket.Types as Types
import Control.Concurrent.Fiber.Network.Internal

foreign import Java unsafe "@static eta.network.Utils.connect"
  c_connect' :: Channel -> SocketAddress -> IO Bool
foreign import Java unsafe "@static eta.network.Utils.accept"
  c_accept' :: Channel -> IO (Maybe Channel)
foreign import Java unsafe "@static eta.runtime.concurrent.waitAccept"
  threadWaitAccept' :: Any -> IO ()
foreign import Java unsafe "@static eta.runtime.concurrent.waitConnect"
  threadWaitConnect' :: Any -> IO ()
foreign import Java unsafe "@static eta.control.concurrent.fiber.network.Utils.setNonBlock"
  setNonBlock' :: Channel -> IO ()

setNonBlock = liftIO setNonBlock'
threadWaitAccept = liftIO threadWaitAccept'
threadWaitConnect = liftIO threadWaitConnect'
c_connect = liftIO c_connect'
c_accept = liftIO c_accept'

newSockAddr x = liftIO $ Types.newSockAddr x

accept :: Socket                        -- Queue Socket
       -> Fiber (Socket,                   -- Readable Socket
              SockAddr)                 -- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readMVar status
 okay <- isAcceptable sock
 if not okay
   then
     ioError $ userError $
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
-- modifyMvar need to be implemented
 modifyMVar_ socketStatus $ \currentStatus -> do
 if shouldError currentStatus
  then
    ioError $ userError $
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
   errLoc = "Network.Socket.connect: " ++ show sock
   shouldError NotConnected = False
   shouldError (Bound _) = False
   shouldError _ = True


readRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> Fiber CInt

writeRawBufferPtr :: String -> FD -> Ptr Word8 -> Int -> CSize -> Fiber CInt

sendAll :: Socket      -- ^ Connected socket
        -> ByteString  -- ^ Data to send
        -> Fiber ()

sendMany :: Socket        -- ^ Connected socket
         -> [ByteString]  -- ^ Data to send
         -> Fiber ()

sendBuf :: Socket     -- Bound/Connected Socket
        -> Ptr Word8  -- Pointer to the data to send
        -> Int        -- Length of the buffer
        -> Fiber Int     -- Number of Bytes sent

send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> Fiber Int      -- ^ Number of bytes sent


