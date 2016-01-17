module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Lazy   as LB
import qualified Data.Map               as M
import Data.ByteString.Builder
import Data.Binary.Get
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

processInstructions :: CurrentState -> MVar Cmd -> IO CurrentState
processInstructions machine mvar = f machine where
    f (CurrentState i st mm)
        | i > 65535 = do {return machine}
        | otherwise =  let
        (m, newState) = interpret machine
        in clean m newState where
            clean Nothing ns = do
                c <- takeMVar mvar
                --print c
                --print $ inst ns
                case c of Go        -> do 
                                        _ <- putMVar mvar c
                                        next <-  processInstructions ns mvar
                                        return next
                          Pause     -> do 
                                        _ <- putMVar mvar c
                                        threadDelay (10^6 * 3)
                                        next <-  processInstructions machine mvar
                                        return next
                          Step       -> do
                                        _ <- putMVar mvar Step
                                        threadDelay (10^6 * 5)
                                        next <- processInstructions ns mvar
                                        print . map (\i-> (memory next) M.! i) $ registers
                                        return next
                          (SetR r v) -> do
                                        _ <- putMVar mvar Pause
                                        let tweaked = CurrentState {
                                            inst = v,
                                            stack = st,
                                            memory = writeTo r v (memory machine)
                                        }
                                        processInstructions tweaked mvar
                          (Break i')  -> if i' == i
                                            then do
                                                _ <- putMVar mvar Pause
                                                next <- processInstructions machine mvar
                                                return next
                                            else do
                                                _ <- putMVar mvar (Break i')
                                                next <- processInstructions ns mvar
                                                return next
                          Quit       -> do { die "Received quit command"}
                
            clean (Just (Dbg c ls)) ns = do
                processInstructions ns mvar
            clean (Just Exit) ns = do
                die "all done" 
            clean (Just (TermIn addr)) ns = do
                
                let h = ord . head $ res
                    input = CurrentState {
                        inst = inst ns,
                        stack = stack ns,
                        memory = writeTo addr (asInt h) (memory ns)
                    }
                putMVar mvar c
                next <- processInstructions input mvar
                return next
            clean (Just (Term c)) ns = do
                x <- putChar . chr . fromIntegral . toInteger $ c 
                next <- processInstructions ns mvar
                return next 

main :: IO ()
main = do 
    contents <- LB.readFile "challenge.bin"
    hSetBuffering stdout NoBuffering
    let codes = runGet toInstructions contents
        zeroes = take 20000 . repeat $ 0
        initialMem = M.fromList . zip [0..] . take ((asInt registerMax) + 1) . concat $ [codes, zeroes]
        initialMachine = CurrentState {inst = 0, stack = [], memory = initialMem }
    mvr <- newEmptyMVar
    putMVar mvr Go
    startDebugger mvr
    res <- processInstructions initialMachine mvr
    print "terminated"
