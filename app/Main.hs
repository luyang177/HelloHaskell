module Main where

import Data.List
import Data.List.Utils

mkString :: String -> [String] -> String
mkString sperator = concatMap (++ sperator)

subIndex2 :: Eq a => [a] -> Int -> [a] -> Maybe Int
subIndex2 searchStr 0 srcList = subIndex searchStr srcList
subIndex2 searchStr startIndex srcList = 
    let restStr = drop startIndex srcList
        result = subIndex searchStr restStr
    in fmap (+ startIndex) result

-- StartIndex -> Length -> src -> dest    
substring :: Int -> Int -> [a] -> [a]
substring startIndex length = take length . drop startIndex

process :: String -> String
process = mkString "\n" . processLines . lines

processLines :: [String] -> [String]
processLines = fmap (\x -> 
    case subIndex "FontSize" x of 
     Just fontSizePos -> getFontSize x fontSizePos 3
     _ -> x)

  
getFontSize :: String -> Int -> Int -> String
getFontSize str fontSizePos offset = 
    let numberStart = fontSizePos + length "FontSize=\""        
        nextSeperator = subIndex2 "\"" numberStart str
    in case nextSeperator of 
        Just nextSeperatorValue -> 
            let value = read $ substring numberStart (nextSeperatorValue - numberStart) str
            in take numberStart str ++ 
               show (value + offset) ++
               drop nextSeperatorValue str
        _ -> str


main :: IO ()
main = do
    text <- readFile "E:\\Civ6_FontStyles_zh_Hans_CN.xml"
    writeFile "E:\\Civ6.xml" (process text)