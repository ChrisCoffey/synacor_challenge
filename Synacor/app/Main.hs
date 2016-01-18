module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString        as BS
import qualified Data.Map               as M
import Data.ByteString.Builder
import Data.Binary.Strict.Get
import Data.Char                        (chr, ord)
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

handleDebug :: MVar Cmd -> (CurrentState, CurrentState) -> IO CurrentState
handleDebug mvar (before, after) = do
    c <- takeMVar mvar
    case c of   Go        -> putMVar mvar c >> return after
                Pause     -> do 
                            putMVar mvar c
                            print . map (\i-> (memory before) M.! i) $ registers
                            threadDelay (10^6 * 3)
                            return before
                Step       -> do
                            putMVar mvar Step
                            print "Registers" 
                            print . map (\i-> (memory after) M.! i) $ registers
                            print "Stack" 
                            print . stack $ after
                            threadDelay (10^6 * 5)
                            return after
                (SetR r v) -> do
                            putMVar mvar Pause
                            let tweaked = CurrentState {
                                inst = inst before,
                                stack = stack before,
                                memory = writeTo r v (memory before)
                            }
                            return tweaked
                (Break i')  -> if i' == (inst before)
                                then putMVar mvar Pause >> return before
                                else putMVar mvar (Break i') >> return after
                Quit       -> do { die "Received quit command"}
    

processInstructions :: CurrentState -> MVar Cmd -> IO CurrentState
processInstructions machine mvar = f machine where
    f (CurrentState i st mm)
        | i > 65535 = do {return machine}
        | otherwise =  let
        (m, newState) = interpret machine
        in clean m newState where
            clean Nothing ns = do
                next <- handleDebug mvar (machine, ns)
                processInstructions next mvar
            clean (Just (Dbg c ls)) ns = processInstructions ns mvar
            clean (Just Exit) ns = die "all done" 
            clean (Just (TermIn addr)) ns = do
                res <- getChar
                print res
                --let h = ord . head $ res
                let input = CurrentState {
                        inst = inst ns,
                        stack = stack ns,
                        memory = writeTo addr (asInt . ord $ res) (memory ns)
                    }
                next <- processInstructions input mvar
                return next
            clean (Just (Term c)) ns = do
                x <- putChar . chr . fromIntegral . toInteger $ c 
                next <- processInstructions ns mvar
                return next 

main :: IO ()
main = do 
    contents <- BS.readFile "challenge.bin"
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin LineBuffering
    let (Right codes, s) = runGet toInstructions contents
        zeroes = take 20000 . repeat $ 0
        initialMem = M.fromList . zip [0..] . take ((asInt registerMax) + 1) $ codes ++ zeroes
        initialMachine = CurrentState {inst = 0, stack = [], memory = initialMem }
    mvr <- newEmptyMVar
    putMVar mvr Go
    startDebugger mvr
    res <- processInstructions initialMachine mvr
    print "terminated"
