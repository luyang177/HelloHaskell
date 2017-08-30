module Main where

import State
import StateSample
import Data.List.Utils
import Data.List

printMaybe :: Show a => Maybe a -> IO ()
printMaybe (Just a) = print a
printMaybe _ = return ()

process :: String -> String
process = concat . processLines . lines

processLines :: [String] -> [String]
processLines aLines = filter (\x -> isInfixOf "FontSize" x) aLines
--printMaybe $ subIndex "foo" "asdfoobar"

main :: IO ()
main = do
    text <- readFile "E:\\Civ6_FontStyles_zh_Hans_CN.xml"
    print $ process text
    --return ()