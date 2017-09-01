module LuyangUtil where

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