module WriterTSample where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Char (isSpace)   

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd str = reverse (dropWhile isSpace (reverse str))

writerTSample1 :: WriterT String (State String) ()
writerTSample1 = do
    put "  ddee    "
    a <- get
    tell $ "Init: " ++ show a ++ "\n"
    modify trimStart
    b <- get
    tell $ "Step 1: " ++ show b ++ "\n"
    modify trimEnd
    c <- get
    tell $ "Step 2: " ++ show c ++ "\n"

runWriterTSample1 :: String
runWriterTSample1 = snd $ evalState (runWriterT writerTSample1) ""    