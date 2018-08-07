{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Recv (
    receive
  , receiveBuf
  , makeReceiveN
  , makePlainReceiveN
  , spell
  ) where

import qualified Control.Concurrent.Fiber.Exception as E
import qualified Control.Exception as IE
import qualified Data.ByteString as BS
import Data.IORef
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
-- import Foreign.ForeignPtr (withForeignPtr)
import Control.Concurrent.Fiber.Network.Internal1 (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
-- import GHC.Conc (threadWaitRead)
-- import Control.Concurrent.Fiber.Network.Internal (threadWaitRead)
-- import Network.Socket (Socket, fdSocket)
import Control.Concurrent.Fiber.Network (Socket)

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

-- #ifdef mingw32_HOST_OS
-- import GHC.IO.FD (FD(..), FDType(..))
import System.Posix.Types (Channel)
import Control.Concurrent.Fiber.Network (recvBuf)
-- import Network.Wai.Handler.Warp.Windows
-- #endif

----------------------------------------------------------------

makeReceiveN :: ByteString -> Recv -> RecvBuf -> Fiber (BufSize -> Fiber ByteString)
makeReceiveN bs0 recv recvBuf = do
    ref <- liftIO $ newIORef bs0
    return $ receiveN ref recv recvBuf

-- | This function returns a receiving function
--   based on two receiving functions.
--   The returned function efficiently manages received data
--   which is initialized by the first argument.
--   The returned function may allocate a byte string with malloc().
makePlainReceiveN :: Socket -> ByteString -> Fiber (BufSize -> Fiber ByteString)
makePlainReceiveN s bs0 = do
    ref <- liftIO $ newIORef bs0
    pool <- newBufferPool
    return $ receiveN ref (receive s pool) (receiveBuf s)

receiveN :: IORef ByteString -> Recv -> RecvBuf -> BufSize -> Fiber ByteString
receiveN ref recv recvBuf size = E.handle (liftIO . handler) $ do
    cached <- liftIO $ readIORef ref
    (bs, leftover) <- spell cached size recv recvBuf
    liftIO $ writeIORef ref leftover
    return bs
 where
   handler :: IE.SomeException -> IO ByteString
   handler _ = return ""

----------------------------------------------------------------

spell :: ByteString -> BufSize -> Fiber ByteString -> RecvBuf -> Fiber (ByteString, ByteString)
spell init0 siz0 recv recvBuf
  | siz0 <= len0 = return $ BS.splitAt siz0 init0
  -- fixme: hard coding 4096
  | siz0 <= 4096 = loop [init0] (siz0 - len0)
  | otherwise    = do
      bs@(PS fptr _ _) <- mallocBS siz0
      withForeignPtr fptr $ \ptr -> do
          ptr' <- copy ptr init0
          full <- recvBuf ptr' (siz0 - len0)
          if full then
              return (bs, "")
            else
              return ("", "") -- fixme
  where
    len0 = BS.length init0
    loop bss siz = do
        bs <- recv
        let len = BS.length bs
        if len == 0 then
            return ("", "")
          else if len >= siz then do
            let (consume, leftover) = BS.splitAt siz bs
                ret = BS.concat $ reverse (consume : bss)
            return (ret, leftover)
          else do
            let bss' = bs : bss
                siz' = siz - len
            loop bss' siz'

receive :: Socket -> BufferPool -> Recv
receive sock pool = withBufferPool pool $ \ (ptr, size) -> do
    let size' = fromIntegral size
    fromIntegral <$> recvBuf sock ptr size'

receiveBuf :: Socket -> RecvBuf
receiveBuf sock buf0 siz0 = loop buf0 siz0
  where
    loop _   0   = return True
    loop buf siz = do
        n <- fromIntegral <$> recvBuf sock buf (fromIntegral siz)
        -- fixme: what should we do in the case of n == 0
        if n == 0 then
            return False
          else
            loop (buf `plusPtr` n) (siz - n)
    -- fd = fdSocket sock

-- receiveloop :: Channel -> Ptr Word8 -> CSize -> Fiber CInt
-- -- receiveloop = undefined
-- receiveloop sock ptr size = do
--     bytes <- fromIntegral <$> readRawBufferPtr "recv" (FD (FDGeneric sock) True)(castPtr ptr) 0 size
--     if bytes == -1 then do
--         errno <- liftIO getErrno
--         if errno == eAGAIN then do
--             threadWaitRead sock
--             receiveloop sock ptr size
--           else
--             liftIO $ throwErrno "receiveloop"
--        else
--         return bytes

-- fixme: the type of the return value
-- foreign import ccall unsafe "recv"
--     c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
