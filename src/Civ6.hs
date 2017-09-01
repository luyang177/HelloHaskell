module Civ6 ( 
    changeCiv6FontSize
) where

import Data.List.Utils    
import LuyangUtil

changeCiv6FontSize :: FilePath -> FilePath -> Int -> IO ()
changeCiv6FontSize srcPath destPath fontSizeOffset = do
    text <- readFile srcPath
    writeFile destPath (process fontSizeOffset text)

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
        