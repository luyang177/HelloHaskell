module Main where

import Data.List
import Data.List.Utils

mkString :: String -> [String] -> String
mkString sperator [] = ""
mkString sperator [x] = x
mkString sperator xs = 
    let sizeExcludeLast = length xs - 1
        major = take sizeExcludeLast xs
        last = drop sizeExcludeLast xs
    in concat $ fmap (++ sperator) major ++ last

subIndex2 :: Eq a => [a] -> Int -> [a] -> Maybe Int
subIndex2 searchStr 0 srcList = subIndex searchStr srcList
subIndex2 searchStr startIndex srcList = 
    let restStr = drop startIndex srcList
        result = subIndex searchStr restStr
    in fmap (+ startIndex) result

-- StartIndex -> Length -> src -> dest    
substring :: Int -> Int -> [a] -> [a]
substring startIndex length = take length . drop startIndex

process :: Int -> String -> String
process offset = mkString "\n" . processLines offset . lines

processLines :: Int -> [String] -> [String]
processLines offset = fmap (\x -> 
    case subIndex "FontSize" x of 
     Just fontSizePos -> makeNewFontLine x (fontSizePos + length "FontSize=\"") offset
     _ -> x)

  
makeNewFontLine :: String -> Int -> Int -> String
makeNewFontLine str fontSizePos offset = 
    let nextSeperator = subIndex2 "\"" fontSizePos str
    in case nextSeperator of 
        Just nextSeperatorValue -> 
            let fontSize = read $ substring fontSizePos (nextSeperatorValue - fontSizePos) str
            in take fontSizePos str ++ 
               show (fontSize + offset) ++
               drop nextSeperatorValue str
        _ -> str


main :: IO ()
main = do
    text <- readFile "E:\\Civ6_FontStyles_zh_Hans_CN.xml"
    writeFile "E:\\Civ6.xml" (process 3 text)