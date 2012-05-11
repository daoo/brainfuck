module Brainfuck.Compiler.Brainfuck (compile) where

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Parser.Brainfuck

compile :: [Brainfuck] -> [IL]
compile []               = []
compile (Repeat l : bs)  = Loop 0 (compile l) : compile bs
compile (Token tok : bs) = tok' : compile bs
  where
    tok' = case tok of
      Plus       -> Set 0 $ Add (Get 0) (Const 1)
      Minus      -> Set 0 $ Add (Get 0) (Const (-1))
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ Get 0
      Input      -> GetChar 0
