-- Echo server program
module Main where

import Control.Monad (unless)
import qualified Data.ByteString as S
import Control.Concurrent.Fiber
import Control.Concurrent.Fiber.Network
import Control.Concurrent.Fiber.Network.Internal (fiber)

main = fiber main'

main' :: Fiber ()
main' = withSocketsDo $
    do addrinfos <- getAddrInfo
                    (Just defaultHints)
                    (Just "127.0.0.1") (Just "3000")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bind sock (addrAddress serveraddr)
       listen sock 1
       (conn, _) <- accept sock
       talk conn
       close conn
       close sock

    where
      talk :: Socket -> Fiber ()
      talk conn =
          do msg <- recv conn 1024
             unless (S.null msg) $ sendAll conn msg >> talk conn
