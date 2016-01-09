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

processInstructions :: Instructions -> CurrentState -> IO CurrentState
processInstructions is s = f s is where
    f (CurrentState i _ _ _) is
        | i > 10000000 = do {return s}
        | otherwise =  let
        code = (M.! i) is
        (m, newState) = interpret s code
        in clean m newState where
            clean Nothing ns = processInstructions is ns
            clean (Just Exit) ns = do { die "all done" }
            clean (Just (Term c)) ns = do 
                x <- putChar $ chr . fromIntegral . toInteger $ c 
                next <- processInstructions is ns
                return next 

main :: IO ()
main = do 
    contents <- LB.getContents
    let codes = runGet (fmap parseOpcodes toInstructions) contents
        instructions = M.fromList . zip [1..] $ codes
        initialMachine = CurrentState {inst = 1, regs = [], stack = [], memory = []}
    x <- print codes
    res <-  processInstructions instructions initialMachine
    print res
