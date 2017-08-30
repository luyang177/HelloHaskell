module Main where

import State
import StateSample

import Data.List
import Data.List.Utils

printMaybe :: Show a => Maybe a -> IO ()
printMaybe (Just a) = print a
printMaybe _ = return ()

process :: String -> String
process = concat . processLines . lines

processLines :: [String] -> [String]
processLines = fmap (\x -> 
    case subIndex "FontSize" x of 
     Just fontSizePos -> 
        let currentFontSize = getFontSize x fontSizePos
            destFontSize = currentFontSize + 3
        in show destFontSize
     _ -> x)

getFontSize :: String -> Int -> Int
getFontSize str fontSizePos = 
    let numberStart = fontSizePos + length "FontSize=\""
        restStr = drop numberStart str
        nextSeperator = elemIndex '\"' restStr
    in case nextSeperator of 
        Just nextSeperatorInt -> read $ take nextSeperatorInt restStr 
        _ -> undefined

--printMaybe $ subIndex "foo" "asdfoobar"

main :: IO ()
main = do
    text <- readFile "E:\\Civ6_FontStyles_zh_Hans_CN.xml"
    print $ process text
    --return ()