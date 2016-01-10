{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Interpreter where

import qualified Data.Map       as M
import qualified Data.List      as L
import Data.Maybe
import Data.Word

import Synacor.Machine
import Synacor.Parser

asInt w = fromInteger . toInteger $ w

writeTo :: Word16 ->  Word16 -> [Word16] -> [Word16]
writeTo i v ls = concat [(take (asInt i) ls), [v], (drop ((asInt i) + 1) ls)]

readFrom :: Word16 -> [Word16] -> Word16
readFrom i ls = ls !! (asInt i)

interpret :: CurrentState -> (Maybe Output, CurrentState)
interpret machine@(CurrentState idx stk mem) = let
    (opcode, length) = parseOpcode . (drop (asInt idx)) $ mem
    nextOp = idx + (fromIntegral length)
    handle = f where
        f Halt = (Just Exit, machine)
        f (Set a b) = let
            valB = readFrom b mem
            newMem = writeTo a valB mem
            in (Nothing, CurrentState {inst = nextOp, stack = stk, memory = newMem} ) 
        f (Out c) =  (Just (Term c), CurrentState {inst = nextOp, stack = stk, memory = mem} )
        f NoOp = (Nothing, machine)
    in handle opcode
