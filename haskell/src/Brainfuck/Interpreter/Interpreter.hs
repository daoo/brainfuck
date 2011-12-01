module Brainfuck.Interpreter.Interpreter where

import Data.Sequence

import Brainfuck.Ext
import Brainfuck.Interpreter.State
import Brainfuck.Compiler.IL

run :: [IL] -> State -> State
run [] state       = state
run (op:ops) state = run ops $ evalOp op state

evalOp :: IL -> State -> State
evalOp (Loop ops) state            = until ((== 0) . current . memory) (run ops) state
evalOp PutChar (State inp out mem) = State inp (out |> current mem) mem
evalOp GetChar (State inp out mem) = State (tail inp) out (modify (const $ head inp) mem)
evalOp op (State inp out mem)      = State inp out (opPure op mem)

opPure :: IL -> Memory -> Memory
opPure tok mem =
  case tok of
    Shift i | i > 0     -> times shiftR mem i
            | i < 0     -> times shiftL mem $ abs i
            | otherwise -> mem
    Poke i              -> modify (+ fromIntegral i) mem
    _                   -> error "Unsupported operation"
