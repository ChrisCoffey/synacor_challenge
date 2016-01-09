module Main where

import qualified Data.ByteString.Lazy   as LB
import qualified Data.Map               as M
import Data.ByteString.Builder
import Data.Binary.Get
import Data.Char                        (chr)
import Data.Monoid
import Data.Word
import System.Exit                      (die)

import Synacor.Parser
import Synacor.Machine
import Synacor.Interpreter

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

processInstructions :: CurrentState -> Instructions -> IO CurrentState
processInstructions s is = f s is where
    f (CurrentState i _ _ _) is = let
        code = (M.! i) is
        (m, newState) = interpret s code
        in clean m newState where
            clean Nothing ns = return ns
            clean (Just Exit) ns = do { die "all done" }
            clean (Just (Term c)) ns = do 
                x <- putChar $ chr . fromIntegral . toInteger $ c 
                return ns

main :: IO ()
main = do 
    contents <- LB.getContents
    let codes = runGet (fmap parseOpcodes toInstructions) contents
        instructions = M.fromList . zip [1..] $ codes
        res = processInstructions instructions
    print res
