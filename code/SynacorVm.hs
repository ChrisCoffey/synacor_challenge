module Main where

import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder
import Data.Binary.Get
import Data.Word
import Data.Monoid

import Synacor.Parser

toInstructions :: Get [Word16]
toInstructions = do
    empty <- isEmpty
    if empty
        then return []
        else do v <- getWord16le
                rest <- toInstructions
                return (v:rest)

joinWords :: [Word16] -> Builder
joinWords [] = mempty
joinWords (w:ws) =
    (word16LE w) <> mconcat [word16LE w' | w' <- ws]

main :: IO ()
main = do 
    contents <- LB.getContents
    print $ runGet (fmap parseOpcodes toInstructions) contents
