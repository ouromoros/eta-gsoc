{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Wai.Handler.Warp.SendFile (
    sendFile
  , readSendFile
  , packHeader -- for testing
  ) where

import qualified Data.ByteString as BS
import Control.Concurrent.Fiber.Network (Socket)

import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (plusPtr)
import qualified System.IO as IO

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Function to send a file based on sendfile() for Linux\/Mac\/FreeBSD.
--   This makes use of the file descriptor cache.
--   For other OSes, this is identical to 'readSendFile'.
--   ** Not implemented yet **
--
-- Since: 3.1.0
sendFile :: Socket -> Buffer -> BufSize -> (ByteString -> Fiber ()) -> SendFile
-- #ifdef SENDFILEFD
-- sendFile s _ _ _ fid off len act hdr = case mfid of
--     -- settingsFdCacheDuration is 0
--     Nothing -> sendfileWithHeader   s path (PartOfFile off len) act hdr
--     Just fd -> sendfileFdWithHeader s fd   (PartOfFile off len) act hdr
--   where
--     mfid = fileIdFd fid
--     path = fileIdPath fid
-- #else
sendFile _ = readSendFile
-- #endif

----------------------------------------------------------------

packHeader :: Buffer -> BufSize -> (ByteString -> Fiber ())
           -> Fiber () -> [ByteString]
           -> Int
           -> Fiber Int
packHeader _   _   _    _    [] n = return n
packHeader buf siz send hook (bs:bss) n
  | len < room = do
      let dst = buf `plusPtr` n
      void $ copy dst bs
      packHeader buf siz send hook bss (n + len)
  | otherwise  = do
      let dst = buf `plusPtr` n
          (bs1, bs2) = BS.splitAt room bs
      void $ copy dst bs1
      bufferIO buf siz send
      hook
      packHeader buf siz send hook (bs2:bss) 0
  where
    len = BS.length bs
    room = siz - n

mini :: Int -> Integer -> Int
mini i n
  | fromIntegral i < n = i
  | otherwise          = fromIntegral n

-- | Function to send a file based on pread()\/send() for Unix.
--   This makes use of the file descriptor cache.
--   For Windows, this is emulated by 'Handle'.
--
-- Since: 3.1.0
-- #ifdef WINDOWS
readSendFile :: Buffer -> BufSize -> (ByteString -> Fiber ()) -> SendFile
readSendFile buf siz send fid off0 len0 hook headers = do
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    liftIO $ IO.withBinaryFile path IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek off0
        n <- IO.hGetBufSome h buf' (mini room len0)
        fiber $ bufferIO buf (hn + n) send
        fiber hook
        let n' = fromIntegral n
        fptr <- newForeignPtr_ buf
        fiber $ loop h fptr (len0 - n')
  where
    path = fileIdPath fid
    loop h fptr len
      | len <= 0  = return ()
      | otherwise = do
        n <- liftIO $ IO.hGetBufSome h buf (mini siz len)
        when (n /= 0) $ do
            let bs = PS fptr 0 n
                n' = fromIntegral n
            send bs
            hook
            loop h fptr (len - n')
