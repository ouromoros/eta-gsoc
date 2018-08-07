{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Date (
    withDateCache
  , GMTDate
  ) where

import Control.AutoUpdate (defaultUpdateSettings, updateAction, mkAutoUpdate)
import Data.ByteString
import Network.HTTP.Date

-- #if WINDOWS
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime(..))
-- #else
-- import System.Posix (epochTime)
-- #endif
import Control.Concurrent.Fiber
import Network.Wai.Handler.Warp.Fiber

-- | The type of the Date header value.
type GMTDate = ByteString

-- | Creating 'DateCache' and executing the action.
withDateCache :: (Fiber GMTDate -> Fiber a) -> Fiber a
withDateCache action = initialize >>= action

initialize :: Fiber (Fiber GMTDate)
initialize = liftIO $ liftIO <$> (mkAutoUpdate defaultUpdateSettings {
                            updateAction = formatHTTPDate <$> getCurrentHTTPDate
                          })

-- #ifdef WINDOWS
uToH :: UTCTime -> HTTPDate
uToH = epochTimeToHTTPDate . CTime . truncate . utcTimeToPOSIXSeconds

getCurrentHTTPDate :: IO HTTPDate
getCurrentHTTPDate =  uToH <$> getCurrentTime
-- #else
-- getCurrentHTTPDate :: IO HTTPDate
-- getCurrentHTTPDate = epochTimeToHTTPDate <$> epochTime
-- #endif
