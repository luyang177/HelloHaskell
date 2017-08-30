module StateSample (
    runStateSample1
) where

import Data.Char (isSpace)    
import State

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd str = reverse (dropWhile isSpace (reverse str))

stateSample1 :: State String ()
stateSample1 = do
    put "  ddee    "
    modify trimStart
    modify trimEnd

runStateSample1 :: ((), String)
runStateSample1 = runState stateSample1 ""    