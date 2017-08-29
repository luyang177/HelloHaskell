module Main where

import State
import StateSample

main :: IO ()
main = do
    let (_, s) = runState stateSample1 ""
    putStrLn s