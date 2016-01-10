{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Machine where

import Data.Maybe
import Data.Word
import qualified Data.Map as M

type Instructions = M.Map Int Opcode
type Registers = M.Map Word16 Word16
type Memory = M.Map Word16 Word16

data CurrentState = 
    CurrentState {
        inst :: Int,
        regs :: Registers,
        stack :: [Word16],
        memory :: Memory
    }
    deriving (Show, Eq)


data Output = 
    Term Word16 |
    Exit |
    Error String


maxAddress = 32767 :: Word16
registers :: Registers
registers = M.fromList $ map (\x-> ((x :: Word16) + maxAddress, 0 :: Word16)) [1..8]
mainMemory :: Memory
mainMemory = M.fromList $ map (\x-> ( (x :: Word16), 0 :: Word16) ) [0..32767]

data Opcode = 
    Halt |                       --0
    Set Word16 Word16 |          --1
    Push Word16 |                --2
    Pop Word16 |                 --3
    Equ Word16 Word16 Word16 |   --4
    Gt Word16 Word16 Word16 |    --5
    Jmp Word16 |                 --6
    Jt Word16 Word16 |           --7
    Jf Word16 Word16 |           --8
    Add Word16 Word16 Word16 |   --9
    Mult Word16 Word16 Word16 |  --10
    Mod Word16 Word16 Word16 |   --11
    And Word16 Word16 Word16 |   --12
    Or Word16 Word16 Word16 |    --13
    Not Word16 Word16 |          --14
    RMem Word16 Word16 |         --15
    WMem Word16 Word16 |         --16
    Call Word16 |                --17
    Ret |                        --18
    Out Word16 |                 --19
    In Word16 |                  --20
    NoOp|                        --21
    Unimplemented Word16         -- ???
    deriving (Show)
