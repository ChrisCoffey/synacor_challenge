{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Interpreter where

import qualified Data.Map       as M
import qualified Data.List      as L
import Data.Maybe
import Data.Word

import Synacor.Machine

asInt w = fromInteger . toInteger $ w

interpret :: CurrentState -> Opcode -> (Maybe Output, CurrentState)
interpret state Halt = (Just Exit, state)
interpret (CurrentState i r s m) NoOp = let
    nm = CurrentState { inst = i + 1, regs = r, stack = s, memory = m }
    in (Nothing, nm)
interpret (CurrentState i r s m) (Out c)= let
    nm = CurrentState { inst = i + 1, regs = r, stack = s, memory = m }
    t = Term c
    in (Just t, nm)
interpret (CurrentState i r s m) (Jmp a) = let
    nm = CurrentState { inst = asInt a, regs = r, stack = s, memory = m }
    t = Term (37 :: Word16)
    in (Just t , nm)
interpret cs@(CurrentState i r s m) (Jf reg targ) = let
    v = (M.! reg) r
    ns = CurrentState { inst = asInt targ, regs = r, stack = s, memory = m }
    t = Term (38 :: Word16)
    in (Just t, if v == 0 then ns else cs)
interpret cs@(CurrentState i r s m) (Jt reg targ) = let
    v = (M.! reg) r
    ns = CurrentState { inst = asInt targ, regs = r, stack = s, memory = m }
    t = Term (38 :: Word16)
    in (Just t, if v == 0 then cs else ns)
interpret (CurrentState i r s m) (Set reg val) = let
    nm = CurrentState {inst = i + 1, regs = M.insert reg val r, stack = s, memory = m }
    in (Nothing, nm)
interpret (CurrentState i r s m) (Call rx) = let
    targ = (M.! rx) r
    nm = CurrentState {inst = asInt targ, regs = r, stack = (fromIntegral (i + 1)):s, memory = m }
    in (Nothing, nm)
interpret cs@(CurrentState i r s m) Ret = let 
    nm = CurrentState {inst = asInt $ head s, regs = r, stack = tail s, memory = m}
    res = if L.null s then (Just Exit, cs) else (Nothing, nm)
    in res
interpret (CurrentState i r s m) _ = let
    nm = CurrentState { inst = i + 1, regs = r, stack = s, memory = m }
    in (Just Exit, nm)
