{-# LANGUAGE RecordWildCards, CPP, DoAndIfThenElse, ForeignFunctionInterface #-}

module Network.Wai.Handler.Warp.FileInfoCache (
    FileInfo(..)
  , Hash
  , withFileInfoCache
  , getInfo -- test purpose only
  ) where

import Control.Exception as E
import qualified Control.Concurrent.Fiber.Exception as FE
import Control.Reaper
import Network.HTTP.Date
-- import System.PosixCompat.Files

import Network.Wai.Handler.Warp.HashMap (HashMap)
import qualified Network.Wai.Handler.Warp.HashMap as M
import Network.Wai.Handler.Warp.Imports
#ifdef INTEROP
import qualified Interop.Java.IO as JIO
#else
import qualified Java.IO as JIO
#endif
import Java

----------------------------------------------------------------

type Hash = Int

-- | File information.
data FileInfo = FileInfo {
    fileInfoName :: !FilePath
  , fileInfoSize :: !Integer
  , fileInfoTime :: HTTPDate   -- ^ Modification time
  , fileInfoDate :: ByteString -- ^ Modification time in the GMT format
  } deriving (Eq, Show)

data Entry = Negative | Positive FileInfo
type Cache = HashMap FilePath Entry
type FileInfoCache = Reaper Cache (Int,FilePath,Entry)

----------------------------------------------------------------

foreign import java unsafe "@new" newFile :: String -> Java a JIO.File
-- newFile = undefined


-- | Getting the file information corresponding to the file.
getInfo :: FilePath -> Fiber FileInfo
getInfo = liftIO . getInfo'

getInfo' :: FilePath -> IO FileInfo
getInfo' path = java $ do
  file <- newFile path
  withObject file $ do 
    regular <- fmap not JIO.isDirectory
    readable <- JIO.canRead
    if (regular && readable) then do
      lastMod <- JIO.lastModified
      let epoch = fromIntegral $ lastMod `div` 1000
          time  = epochTimeToHTTPDate epoch
      size <- fmap fromIntegral $ JIO.length
      let date = formatHTTPDate time
      return $ FileInfo { fileInfoName = path
                        , fileInfoSize = size
                        , fileInfoTime = time
                        , fileInfoDate = date }
    else do
      absolutePath <- file <.> JIO.getAbsolutePath
      io $ throwIO (userError $ "File:getInfo: " ++ absolutePath)

getInfoNaive :: Hash -> FilePath -> Fiber FileInfo
getInfoNaive _ = getInfo

----------------------------------------------------------------

getAndRegisterInfo :: FileInfoCache -> Hash -> FilePath -> Fiber FileInfo
getAndRegisterInfo reaper@Reaper{..} h path = do
    cache <- liftIO reaperRead
    case M.lookup h path cache of
        Just Negative     -> liftIO $ throwIO (userError "FileInfoCache:getAndRegisterInfo")
        Just (Positive x) -> return x
        Nothing           -> liftIO (positive reaper h path
                               `onException` negative reaper h path)

positive :: FileInfoCache -> Hash -> FilePath -> IO FileInfo
positive Reaper{..} h path = do
    info <- getInfo' path
    reaperAdd (h, path, Positive info)
    return info

negative :: FileInfoCache -> Hash -> FilePath -> IO FileInfo
negative Reaper{..} h path = do
    reaperAdd (h, path,Negative)
    throwIO (userError "FileInfoCache:negative")

----------------------------------------------------------------

-- | Creating a file information cache
--   and executing the action in the second argument.
--   The first argument is a cache duration in second.
withFileInfoCache :: Int
                  -> ((Hash -> FilePath -> Fiber FileInfo) -> Fiber a)
                  -> Fiber a
withFileInfoCache 0        action = action getInfoNaive
withFileInfoCache duration action =
    FE.bracket (liftIO $ initialize duration)
              (liftIO . terminate)
              (action . getAndRegisterInfo)

initialize :: Hash -> IO FileInfoCache
initialize duration = mkReaper settings
  where
    settings = defaultReaperSettings {
        reaperAction = override
      , reaperDelay  = duration
      , reaperCons   = \(h,k,v) -> M.insert h k v
      , reaperNull   = M.null
      , reaperEmpty  = M.empty
      }

override :: Cache -> IO (Cache -> Cache)
override _ = return $ const M.empty

terminate :: FileInfoCache -> IO ()
terminate x = liftIO $ void $ reaperStop x
