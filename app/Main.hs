module Main where

import qualified Data.ByteString as B
import State
import StateSample

main :: IO ()
main = do
    let (_, s) = runState stateSample1 ""
    putStrLn s
    --let ss = B.pack "Hello, world"
    --putStrLn "ddd"
    