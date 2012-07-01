module Brainfuck.Compiler.Brainfuck (compile) where

import Brainfuck.Data.Brainfuck
import Brainfuck.Data.Expr
import Brainfuck.Data.IL

compile :: [Brainfuck] -> [IL]
compile []               = []
compile (Repeat l : bs)  = While (Get 0) (compile l) : compile bs
compile (Token tok : bs) = tok' : compile bs
  where
    tok' = case tok of
      Plus       -> Set 0 $ Add (Get 0) (Const 1)
      Minus      -> Set 0 $ Add (Get 0) (Const (-1))
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ Get 0
      Input      -> GetChar 0
