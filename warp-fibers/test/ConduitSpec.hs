{-# LANGUAGE OverloadedStrings #-}
module ConduitSpec (main, spec) where

import Network.Wai.Handler.Warp.Conduit
import Network.Wai.Handler.Warp.Types
import Control.Monad (replicateM)
import Test.Hspec
import Data.IORef as I
import qualified Data.ByteString as S
import Network.Wai.Handler.Warp.Fiber
import Control.Concurrent.Fiber

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "conduit" $ do
    it "IsolatedBSSource" $ do
        ref <- newIORef $ map S.singleton [1..50]
        src <- fiber $ mkSource $ do
            x <- liftIO $ readIORef ref
            case x of
                [] -> return S.empty
                y:z -> do
                    liftIO $ writeIORef ref z
                    return y
        isrc <- fiber $ mkISource src 40
        x <- fiber $ replicateM 20 $ readISource isrc
        S.concat x `shouldBe` S.pack [1..20]

        y <- fiber $ replicateM 40 $ readISource isrc
        S.concat y `shouldBe` S.pack [21..40]

        z <- fiber $ replicateM 40 $ readSource src
        S.concat z `shouldBe` S.pack [41..50]
    it "chunkedSource" $ do
        ref <- newIORef $ "5\r\n12345\r\n3\r\n678\r\n0\r\n\r\nBLAH"
        src <- fiber $ mkSource $ do
            x <- liftIO $ readIORef ref
            liftIO $ writeIORef ref S.empty
            return x
        csrc <- fiber $ mkCSource src

        x <- fiber $ replicateM 15 $ readCSource csrc
        S.concat x `shouldBe` "12345678"

        y <- fiber $ replicateM 15 $ readSource src
        S.concat y `shouldBe` "BLAH"
    it "chunk boundaries" $ do
        ref <- newIORef
            [ "5\r\n"
            , "12345\r\n3\r"
            , "\n678\r\n0\r\n"
            , "\r\nBLAH"
            ]
        src <- fiber $ mkSource $ do
            x <- liftIO $ readIORef ref
            case x of
                [] -> return S.empty
                y:z -> do
                    liftIO $ writeIORef ref z
                    return y
        csrc <- fiber $ mkCSource src

        x <- fiber $ replicateM 15 $ readCSource csrc
        S.concat x `shouldBe` "12345678"

        y <- fiber $ replicateM 15 $ readSource src
        S.concat y `shouldBe` "BLAH"
