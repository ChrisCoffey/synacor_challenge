module Main where

import Control.Concurrent
import qualified Data.ByteString.Lazy   as LB
import qualified Data.Map               as M
import Data.ByteString.Builder
import Data.Binary.Get
import Data.Char                        (chr)
import Data.Monoid
import Data.Word
import System.IO
import System.Exit                      (die)

import Synacor.Parser
import Synacor.Machine
import Synacor.Interpreter
import Synacor.Debugger

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

processInstructions :: CurrentState -> IO CurrentState
processInstructions machine = f machine where
    f (CurrentState i _ _) 
        | i > 65535 = do {return machine}
        | otherwise =  let
        (m, newState) = interpret machine
        in clean m newState where
            clean Nothing ns = do
                --_ <- print $ (inst ns, stack ns, (drop (asInt maxAddress) (memory ns)))
                processInstructions ns
            clean (Just (Dbg c ls)) ns = do
                --_ <- print c
                --_ <- print ls
                processInstructions ns
            clean (Just Exit) ns = do { die "all done" }
            clean (Just (Term c)) ns = do 
                x <- putChar $ chr . fromIntegral . toInteger $ c 
                next <- processInstructions ns
                return next 


main :: IO ()
main = do 
    contents <- LB.getContents
    let codes = runGet toInstructions contents
        zeroes = take 20000 . repeat $ 0
        initialMem = take ((asInt registerMax) + 1) . concat $ [codes, zeroes]
        initialMachine = CurrentState {inst = 0, stack = [], memory = initialMem }
    _ <- mapM_ print $ zip [0..] initialMem
    mvr <- newEmptyMVar
    startDebugger mvr
    res <- processInstructions initialMachine
    print "terminated"
