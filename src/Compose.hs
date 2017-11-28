module Compose where

import Data.Function
import Control.Category

f size = "dd" ++ show size

compose1 :: String -> String
compose1 str = 
            str 
            & length
            & f

compose2 :: String -> String
compose2 = length >>> f

    