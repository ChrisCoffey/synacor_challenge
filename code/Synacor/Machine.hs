{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Machine where

import Data.Maybe
import Data.Word

data CurrentState = 
    CurrentState {
        regs :: [Word16],
        stack :: [Word16],
        memory :: [Word16]
    }

maxAddress = 32767
registers = map (+ maxAddress) [1..8]

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
    Ret Word16 |                 --18
    Out Word16 |                 --19
    In Word16 |                  --20
    NoOp|                        --21
    Unimplemented
    deriving (Show)
