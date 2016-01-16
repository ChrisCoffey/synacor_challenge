module Main where

import Control.Concurrent
import Control.Concurrent.MVar
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
                --print $ (inst ns, stack ns, (drop (asInt (1 + maxAddress)) (memory ns)))
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
                                        return next
                          (SetR r i) -> do
                                        _ <- putMVar mvar Pause
                                        let tweaked = CurrentState {
                                            inst = i,
                                            stack = st,
                                            memory = concat [(take (asInt r) mm), [i], (drop ((asInt i) + 1) mm)]
                                        }
                                        processInstructions tweaked mvar
                          (Break i)  -> if inst ns == i
                                            then do
                                                _ <- putMVar mvar Pause
                                                print . drop (asInt (maxAddress + 1)) . memory $ machine
                                                next <- processInstructions machine mvar
                                                return next
                                            else do
                                                _ <- putMVar mvar Go
                                                next <- processInstructions ns mvar
                                                return next
                          Quit       -> do { die "Received quit command"}
                
            clean (Just (Dbg c ls)) ns = do
                --_ <- print c
                --_ <- print ls
                processInstructions ns mvar
            clean (Just Exit) ns = do  
                die "all done" 
            clean (Just (Term c)) ns = do 
                x <- putChar $ chr . fromIntegral . toInteger $ c 
                next <- processInstructions ns mvar
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
    putMVar mvr Pause
    startDebugger mvr
    res <- processInstructions initialMachine mvr
    print "terminated"
