module Brainfuck.Interpreter.NewInterpreter where

import Brainfuck.Interpreter.Memory
import Brainfuck.Interpreter.Ext
import Brainfuck.Parser.Parser

asdf str = eval (parse str) ([], [], newMemory)

eval (Sequence tok exp) s@(inp, out, mem) =
  case tok of
    Output -> eval exp (tail inp, current mem : out, mem)
    Input  -> eval exp (tail inp, out, modify (const $ head inp) mem)
    _      -> eval exp (inp, out, tokenPure mem tok)
eval (Loop exp) mem = until (\(_, _, mem) -> current mem == 0) (eval exp) mem

tokenPure :: Memory -> Token -> Memory
tokenPure mem tok =
  case tok of
    Next     -> shiftL mem
    Previous -> shiftR mem
    Plus     -> modify inc mem
    Minus    -> modify dec mem
