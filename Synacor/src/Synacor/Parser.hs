{-# LANGUAGE ScopedTypeVariables #-}
module Synacor.Parser where

import Data.Word

import Synacor.Machine

parseOpcode :: [Word16] -> (Opcode, Int)
parseOpcode (0:rest) = (Halt, 1)
parseOpcode (1:a:b:rest) = (Set a b, 3)
parseOpcode (2:a:rest) = (Push a, 2)
parseOpcode (3:a:rest) = (Pop a, 2)
parseOpcode (4:a:b:c:rest) = (Equ a b c, 4)
parseOpcode (5:a:b:c:rest) = (Gt a b c, 4)
parseOpcode (6:a:rest) = (Jmp a, 2)
parseOpcode (7:a:b:rest) = (Jt a b, 3)
parseOpcode (8:a:b:rest) = (Jf a b, 3)
parseOpcode (9:a:b:c:rest) = (Add a b c, 4)
parseOpcode (10:a:b:c:rest) = (Mult a b c, 4)
parseOpcode (11:a:b:c:rest) = (Mod a b c, 4)
parseOpcode (12:a:b:c:rest) = (And a b c, 4)
parseOpcode (13:a:b:c:rest) = (Or a b c, 4)
parseOpcode (14:a:b:rest) = (Not a b, 3)
parseOpcode (15:a:b:rest) = (RMem a b, 3)
parseOpcode (16:a:b:rest) = (WMem a b, 3)
parseOpcode (17:a:rest) = (Call a, 2)
parseOpcode (18:rest) = (Ret, 1)
parseOpcode (19:c:rest) = (Out c, 2)
parseOpcode (20:c:rest) = (In c, 2)
parseOpcode (21:rest) = (NoOp, 1)
parseOpcode (r:rest) = (Unimplemented r, 1)
