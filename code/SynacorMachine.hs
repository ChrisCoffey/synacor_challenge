{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Machine where

import System.Exit
import Data.Maybe
import qualified Data.Map as M

data CurrentState = 
    CurrentState {
        registers :: [Int],
        stack :: [Int],
        memory :: [Int]
    }

writeRegister :: CurrentState -> Int -> Int -> Maybe CurrentState
writeRegister (CurrentState rs _ _) r v = let
    reg = n - maxAddress
    rs' = (take reg )

readRegister :: CurrentState -> Int -> Maybe Int
readRegister (CurrentState rs _ _) n = let
    reg = n - maxAddress
    in if reg >= 0 && reg < 8 then Just (rs !! n) else Nothing

maxAddress = 32767
registers = map (+ maxAddress) [1..8]

data Opcode = 
    Halt |
    Set Int Int |
    Push Int |
    Pop Int |
    Equ Int |
    Gt Int |
    Jmp Int |
    Jt Int Int |
    Jf Int Int |
    Add Int Int Int |
    Mult Int Int Int |
    Mod Int Int Int |
    And Int Int Int |
    Or Int Int Int |
    Not Int Int |
    RMem Int Int |
    WMem Int Int |
    Call Int |
    Ret Int |
    Out Int |
    In Int |
    NoOp

