{-# LANGUAGE ScopedTypeVariables #-}

module Synacor.Interpreter where

import Data.Maybe
import Data.Word

import Synacor.Machine

interpret :: CurrentState -> Opcode -> (Maybe Output, CurrentState)
interpret state Halt = (Just Exit, state)
interpret (CurrentState i r s m) NoOp = let
    nm = CurrentState { inst = i + 1, regs = r, stack = s, memory = m }
    in (Nothing, nm)
interpret (CurrentState i r s m) (Out c)= let
    nm = CurrentState { inst = i + 1, regs = r, stack = s, memory = m }
    t = Term c
    in (Just t, nm)
interpret state _ = (Just Exit, state)

