{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.Buffer (
    bufferSize
  , allocateBuffer
  , freeBuffer
  , mallocBS
  , newBufferPool
  , withBufferPool
  , toBuilderBuffer
  , toBuilderBuffer'
  , copy
  , bufferIO
  , copy'
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (memcpy)
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Streaming.ByteString.Builder.Buffer as B (Buffer (..))
import Foreign.Marshal.Alloc (mallocBytes, free, finalizerFree)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (newForeignPtr, newForeignPtr_)
import Control.Concurrent.Fiber.Network.Internal1 (withForeignPtr)
import qualified Foreign.ForeignPtr as F

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | The default size of the write buffer: 16384 (2^14 = 1024 * 16).
--   This is the maximum size of TLS record.
--   This is also the maximum size of HTTP/2 frame payload
--   (excluding frame header).
bufferSize :: BufSize
bufferSize = 16384

-- | Allocating a buffer with malloc().
allocateBuffer :: Int -> Fiber Buffer
allocateBuffer = liftIO . mallocBytes

-- | Releasing a buffer with free().
freeBuffer :: Buffer -> Fiber ()
freeBuffer = liftIO . free

----------------------------------------------------------------

largeBufferSize :: Int
largeBufferSize = 16384

minBufferSize :: Int
minBufferSize = 2048

newBufferPool :: Fiber BufferPool
newBufferPool = liftIO $ newIORef BS.empty

mallocBS :: Int -> Fiber ByteString
mallocBS size = do
    ptr <-  allocateBuffer size
    fptr <- liftIO $ newForeignPtr finalizerFree ptr
    return $! PS fptr 0 size
{-# INLINE mallocBS #-}

usefulBuffer :: ByteString -> Bool
usefulBuffer buffer = BS.length buffer >= minBufferSize
{-# INLINE usefulBuffer #-}

getBuffer :: BufferPool -> Fiber ByteString
getBuffer pool = do
    buffer <- liftIO $ readIORef pool
    if usefulBuffer buffer then return buffer else mallocBS largeBufferSize
{-# INLINE getBuffer #-}

putBuffer :: BufferPool -> ByteString -> Fiber ()
putBuffer pool buffer = liftIO $ writeIORef pool buffer
{-# INLINE putBuffer #-}

withForeignBuffer :: ByteString -> ((Buffer, BufSize) -> Fiber Int) -> Fiber Int
withForeignBuffer (PS ps s l) f = withForeignPtr ps (\p -> f (castPtr p `plusPtr` s, l))
{-# INLINE withForeignBuffer #-}

withBufferPool :: BufferPool -> ((Buffer, BufSize) -> Fiber Int) -> Fiber ByteString
withBufferPool pool f = do
    buffer <- getBuffer pool
    consumed <- withForeignBuffer buffer f
    putBuffer pool $! unsafeDrop consumed buffer
    return $! unsafeTake consumed buffer
{-# INLINE withBufferPool #-}

----------------------------------------------------------------
--
-- Utilities
--

toBuilderBuffer :: Buffer -> BufSize -> Fiber B.Buffer
toBuilderBuffer ptr size = do
    fptr <- liftIO $ newForeignPtr_ ptr
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)

toBuilderBuffer' :: Buffer -> BufSize -> IO B.Buffer
toBuilderBuffer' ptr size = do
    fptr <- newForeignPtr_ ptr
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)

-- | Copying the bytestring to the buffer.
--   This function returns the point where the next copy should start.
copy :: Buffer -> ByteString -> Fiber Buffer
copy !ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    liftIO $ memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $! ptr `plusPtr` l
{-# INLINE copy #-}

copy' :: Buffer -> ByteString -> IO Buffer
copy' !ptr (PS fp o l) = F.withForeignPtr fp $ \p -> do
    memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $! ptr `plusPtr` l
{-# INLINE copy' #-}

bufferIO :: Buffer -> Int -> (ByteString -> Fiber ()) -> Fiber ()
bufferIO ptr siz io = do
    fptr <- liftIO $ newForeignPtr_ ptr
    io $ PS fptr 0 siz
