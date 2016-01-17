{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Interpreter where

import qualified Data.Map       as M
import qualified Data.List      as L
import Data.Bits
import Data.Maybe
import Data.Word

import Synacor.Machine
import Synacor.Parser

asInt w = fromInteger . toInteger $ w

writeTo :: Word16 -> Word16 -> Memory -> Memory
writeTo k v mem = M.insert k v mem

readFrom :: Word16 -> Memory -> Word16
readFrom i mem 
    | i <= maxAddress = i
    | otherwise =  (M.! asInt i) mem

interpret :: CurrentState -> (Maybe Output, CurrentState)
interpret machine@(CurrentState idx stk mem) = let
    memList = map snd . M.toList $ mem
    (opcode, length) = parseOpcode . (drop (asInt idx)) $ memList
    nextOp = idx + (fromIntegral length)
    handle = f where
        --0
        f Halt = (Just Exit, machine)
        --1
        f (Set a b) = let
            valB = readFrom b mem
            newMem = writeTo a valB mem
            in (Nothing, CurrentState {inst = nextOp, stack = stk, memory = newMem} ) 
        --2
        f (Push a) = let
            a' = readFrom a mem 
            in (Nothing, CurrentState {inst = nextOp, stack = a':stk, memory = mem })
        --3
        f (Pop a) = let
            newMem = writeTo a (head stk) mem
            res = if L.null stk then Just Exit else Nothing
            in (res ,  CurrentState {inst = nextOp, stack = tail stk, memory = newMem} )
        --4
        f (Equ a b c) = let
            b' = readFrom b mem 
            c' = readFrom c mem
            newR = if b' == c' then 1 else 0
            newMem = writeTo a newR mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --5
        f (Gt a b c) = let
            b' = readFrom b mem 
            c' = readFrom c mem
            newR = if b' > c' then 1 else 0
            newMem = writeTo a newR mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --6
        f (Jmp a) =  let
            a' = readFrom a mem
            in (Nothing, CurrentState {inst = a', stack = stk, memory = mem} )
        --7
        f (Jt a b) = let
            a' = readFrom a mem
            b' = readFrom b mem
            ni = if a' > 0 then b' else nextOp
            in (Nothing, CurrentState {inst = ni, stack = stk, memory = mem} )
        --8
        f (Jf a b) = let
            a' = readFrom a mem
            b' = readFrom b mem
            ni = if a' == 0 then b' else nextOp
            in (Nothing, CurrentState {inst = ni, stack = stk, memory = mem} )
        --9
        f (Add a b c) = let
            b' = readFrom b mem
            c' = readFrom c mem
            res = (b' + c') `mod` (32768 :: Word16)
            newMem = writeTo a res mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --10
        f (Mult a b c) = let
            b' = readFrom b mem
            c' = readFrom c mem
            res = (b' * c') `mod` (32768 :: Word16)
            newMem = writeTo a res mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --11
        f (Mod a b c) = let
            b' = readFrom b mem
            c' = readFrom c mem
            res = b' `mod` c' 
            newMem = writeTo a res mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --12
        f (And a b c) = let --todo add helpers to reduce all this boiler plate
            b' = readFrom b mem
            c' = readFrom c mem
            res = (.&.) b' c'
            newMem = writeTo a res mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --13
        f (Or a b c) = let
            b' = readFrom b mem
            c' = readFrom c mem
            res = (.|.) b' c'
            newMem = writeTo a res mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --14
        f (Not a b) = let
            b' = readFrom b mem
            res = (complement b') `xor` (32768 :: Word16)
            newMem = writeTo a res mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --15
        f (RMem a b) = let
            isReg = maxAddress < b
            b' = if isReg then (M.! (asInt (readFrom b mem))) mem else (M.! (asInt b)) mem 
            newMem = writeTo a b' mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --16
        f (WMem a b) = let
            isReg = maxAddress < a
            a' = if isReg then readFrom a mem else a
            b' = readFrom b mem
            newMem = writeTo a' b' mem
            in (Nothing,  CurrentState {inst = nextOp, stack = stk, memory = newMem} )
        --17
        f (Call a) = let
            a' = readFrom a mem
            in (Nothing, CurrentState {inst = a', stack = nextOp:stk, memory = mem })
        --18
        f Ret = (if null stk then Just Exit else Nothing, CurrentState {inst = head stk, stack = tail stk, memory = mem })
        --19
        f (Out c) =  (Just (Term c), CurrentState {inst = nextOp, stack = stk, memory = mem} )
        --20
        --21
        f NoOp = (Nothing, CurrentState {inst = nextOp, stack = stk, memory = mem} )
        --unknown
        f x = (Just Exit, CurrentState {inst = nextOp, stack = stk, memory = mem} )
    in handle opcode
