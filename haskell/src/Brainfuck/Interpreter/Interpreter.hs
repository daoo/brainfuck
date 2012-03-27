module Brainfuck.Interpreter.Interpreter where

import Data.Sequence

import Brainfuck.Ext
import Brainfuck.Interpreter.State
import Brainfuck.Compiler.IL

run :: (Integral a) => State a -> [IL] -> State a
run = foldl evalOp

evalOp :: (Integral a) => State a -> IL -> State a
evalOp state (Loop ops)                = until ((== 0) . current . memory) (`run` ops) state
evalOp (State inp out mem) (PutChar 0) = State inp (out |> current mem) mem
evalOp (State inp out mem) (GetChar 0) = State (tail inp) out (modify (const $ head inp) mem)
evalOp (State inp out mem) op          = State inp out (opPure op mem)

opPure :: (Integral a) => IL -> ([a], [a]) -> ([a], [a])
opPure tok mem =
  case tok of
    Shift s -> case s of
      ShiftLeft i -> times shiftL mem i
      ShiftRight i -> times shiftR mem i

    Poke 0 i -> modify (+ fromIntegral i) mem
    _        -> error "Unsupported operation"
