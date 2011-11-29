module Brainfuck.Interpreter.NewInterpreter where

import Brainfuck.Interpreter.Memory
import Brainfuck.Interpreter.Ext
import Brainfuck.Parser.Parser

eval :: (Input, Memory) -> Expr -> Output
eval _ NOP = []
eval s@(inp, mem) (Sequence tok exp) =
  case tok of
    Output -> current mem : eval (tail inp, mem) exp
    Input  -> eval (tail inp, modify (const $ head inp) mem) exp
    _      -> eval (inp, tokenPure mem tok) exp
eval s@(_, mem) (Loop exp) | current mem == 0 = []
                           | otherwise        = eval s exp

tokenPure :: Memory -> Token -> Memory
tokenPure mem tok =
  case tok of
    Next     -> shiftL mem
    Previous -> shiftR mem
    Plus     -> modify inc mem
    Minus    -> modify dec mem
