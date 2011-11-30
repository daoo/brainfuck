module Brainfuck.Interpreter.Interpreter where

import Brainfuck.Interpreter.Memory
import Brainfuck.Interpreter.Ext
import Brainfuck.Parser.Parser

run :: [Expr] -> State -> State
run exps state = foldr evalExpr state exps

evalExpr :: Expr -> State -> State
evalExpr (Token tok) s = evalToken tok s
evalExpr (Loop exp) s  = until ((== 0) . current . thrd) (run exp) s

evalToken :: Token -> State -> State
evalToken Output (inp, out, mem) = (inp, current mem : out, mem)
evalToken Input (inp, out, mem)  = (tail inp, out, modify (const $ head inp) mem)
evalToken tok (inp, out, mem)    = (inp, out, tokenPure tok mem)

tokenPure :: Token -> Memory -> Memory
tokenPure tok mem =
  case tok of
    Next     -> shiftL mem
    Previous -> shiftR mem
    Plus     -> modify inc mem
    Minus    -> modify dec mem
