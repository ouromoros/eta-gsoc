{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_src (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/chaos/.etlas/bin"
libdir     = "/home/chaos/.etlas/lib/eta-0.7.2.1/src-0.1.0.0-inplace-src"
dynlibdir  = "/home/chaos/.etlas/lib/eta-0.7.2.1"
datadir    = "/home/chaos/.etlas/share/eta-0.7.2.1/src-0.1.0.0"
libexecdir = "/home/chaos/.etlas/libexec"
sysconfdir = "/home/chaos/.etlas/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "src_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "src_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "src_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "src_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "src_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "src_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
