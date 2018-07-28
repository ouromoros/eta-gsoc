module Main where

import Control.Concurrent.Fiber
import Control.Monad

main :: IO ()
main = void $ runFiber (return ())
