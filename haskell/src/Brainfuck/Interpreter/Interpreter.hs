module Brainfuck.Interpreter.Interpreter where

import Data.Sequence

import Brainfuck.Ext
import Brainfuck.Interpreter.State
import Brainfuck.Compiler.IL

run :: (Integral a) => State a -> [IL] -> State a
run = foldl evalOp

evalOp :: (Integral a) => State a -> IL -> State a
evalOp state (Loop ops)                = until ((== 0) . current . memory) (`run` ops) state
evalOp (State inp out mem) (PutChar d) = State inp (out |> offset d mem) mem
evalOp (State inp out mem) (GetChar d) = State (tail inp) out (modify' (const $ head inp) d mem)
evalOp (State inp out mem) op          = State inp out mem'
  where
    mem' = case op of
      Poke d i -> modify' (+ fromIntegral i) d mem
      Shift s  -> case s of
        ShiftLeft i  -> times shiftL i mem
        ShiftRight i -> times shiftR i mem
