module Brainfuck.Interpreter.Interpreter where

import Data.Sequence

import Brainfuck.Ext
import Brainfuck.Interpreter.State
import Brainfuck.Compiler.IL

run :: (Integral a) => [IL] -> State a -> State a
run [] state       = state
run (op:ops) state = run ops $ evalOp op state

evalOp :: (Integral a) => IL -> State a -> State a
evalOp (Loop ops) state                = until ((== 0) . current . memory) (run ops) state
evalOp (PutChar 0) (State inp out mem) = State inp (out |> current mem) mem
evalOp (GetChar 0) (State inp out mem) = State (tail inp) out (modify (const $ head inp) mem)
evalOp op (State inp out mem)          = State inp out (opPure op mem)

opPure :: (Integral a) => IL -> ([a], [a]) -> ([a], [a])
opPure tok mem =
  case tok of
    RightShifts i -> times shiftR mem i
    LeftShifts i  -> times shiftL mem i
    Poke 0 i      -> modify (+ fromIntegral i) mem
    _             -> error "Unsupported operation"
