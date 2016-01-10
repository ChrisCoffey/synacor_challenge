{-# LANGUAGE ScopedTypeVariables #-}
module Synacor.Parser where

import Data.Word

import Synacor.Machine

parseOpcodes :: [Word16] -> [Opcode]
parseOpcodes [] = []
parseOpcodes raw = let
    (code, rest) = parseOpcode raw
    in code:(parseOpcodes rest)

parseOpcode :: [Word16] -> (Opcode, [Word16])
parseOpcode (0:rest) = (Halt, rest)
parseOpcode (1:a:b:rest) = (Set a b, rest)
parseOpcode (2:a:rest) = (Push a, rest)
parseOpcode (3:a:rest) = (Pop a, rest)
parseOpcode (4:a:b:c:rest) = (Equ a b c, rest)
parseOpcode (5:a:b:c:rest) = (Gt a b c, rest)
parseOpcode (6:a:rest) = (Jmp a, rest)
parseOpcode (7:a:b:rest) = (Jt a b, rest)
parseOpcode (8:a:b:rest) = (Jf a b, rest)
parseOpcode (9:a:b:c:rest) = (Add a b c, rest)
parseOpcode (10:a:b:c:rest) = (Mult a b c, rest)
parseOpcode (11:a:b:c:rest) = (Mod a b c, rest)
parseOpcode (12:a:b:c:rest) = (And a b c, rest)
parseOpcode (13:a:b:c:rest) = (Or a b c, rest)
parseOpcode (14:a:b:rest) = (Not a b, rest)
parseOpcode (15:a:b:rest) = (RMem a b, rest)
parseOpcode (16:a:b:rest) = (WMem a b, rest)
parseOpcode (17:a:rest) = (Call a, rest)
parseOpcode (18:rest) = (Ret, rest)
parseOpcode (19:c:rest) = (Out c, rest)
parseOpcode (20:c:rest) = (In c, rest)
parseOpcode (21:rest) = (NoOp, rest)
parseOpcode (r:rest) = (Unimplemented r, rest)
