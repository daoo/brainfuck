module Brainfuck.Interpreter.Interpreter where

import Data.Sequence

import Brainfuck.Ext
import Brainfuck.Interpreter.State
import Brainfuck.Compiler.IL

run :: (Integral a) => State a -> [IL] -> State a
run = foldl evalOp

evalOp :: (Integral a) => State a -> IL -> State a
evalOp state (Loop i ops)     = until ((== 0) . offset i . getMemory) (`run` ops) state
evalOp (State inp out mem) op = state'
  where
    state' = case op of
      PutChar d           -> State inp (out |> offset d mem) mem
      GetChar d           -> State (tail inp) out (modify (const $ head inp) d mem)
      Poke d i            -> State inp out $ modify (+ fromIntegral i) d mem
      Shift s | s < 0     -> State inp out $ times shiftL (abs s) mem
              | s > 0     -> State inp out $ times shiftR s mem
              | otherwise -> State inp out mem
      _                   -> error "Should not happen"
