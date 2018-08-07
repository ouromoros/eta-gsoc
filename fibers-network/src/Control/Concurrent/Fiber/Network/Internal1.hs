module Control.Concurrent.Fiber.Network.Internal1
  where

-- | This module contains some `with*` functions that must be rewritten into `Fiber` type

import Control.Concurrent.Fiber
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr
import GHC.ForeignPtr (touchForeignPtr, unsafeForeignPtrToPtr)
import Data.ByteString
import Data.ByteString.Internal (create, memcpy, mallocByteString, ByteString(..))
import Foreign.C.String         (CString, CStringLen)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes, free, finalizerFree)
import Foreign.Ptr (castPtr, plusPtr)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (assert)

withForeignPtr :: ForeignPtr a -> (Ptr a -> Fiber b) -> Fiber b
withForeignPtr fo io = do
    r <- io (unsafeForeignPtrToPtr fo)
    liftIO $ touchForeignPtr fo
    return r

createAndTrim :: Int -> (Ptr Word8 -> Fiber Int) -> Fiber ByteString
createAndTrim l f = do
    fp <- liftIO $ mallocByteString l
    withForeignPtr fp $ \p -> do
        l' <- f p
        if assert (l' <= l) $ l' >= l
            then return $! PS fp 0 l
            else liftIO $ create l' $ \p' -> memcpy p' p (fromIntegral l')

unsafeUseAsCStringLen :: ByteString -> (CStringLen -> Fiber a) -> Fiber a
unsafeUseAsCStringLen (PS ps s l) f = withForeignPtr ps $ \p -> f (castPtr p `plusPtr` s,l)
