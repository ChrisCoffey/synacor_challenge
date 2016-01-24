{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Machine where

import Data.Word
import qualified Data.Map as M

type MStack = [Word16]
type Memory = M.Map Word16 Word16

data CurrentState = 
    CurrentState {
        inst :: Word16,
        stack :: MStack,
        memory :: Memory
    }
    deriving (Show, Eq, Read)

data Output = 
    Term Word16 |
    TermIn Word16 |
    Dbg Opcode [Word16] |
    Exit |
    Error String

maxAddress :: Word16
maxAddress = 32767 :: Word16
registerMax :: Word16
registerMax = maxAddress + 8

registers :: [Word16]
registers = [(maxAddress + 1)..registerMax]

isLiteral :: Word16 -> Bool
isLiteral a = a > 0 && a < maxAddress 

isRegister :: Word16 -> Bool
isRegister a = a > maxAddress && a <= registerMax

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
