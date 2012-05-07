module Brainfuck.Compiler.Brainfuck (compile) where

import qualified Brainfuck.Compiler.Expr as Expr
import Brainfuck.Compiler.IL
import Brainfuck.Parser.Brainfuck

compile :: [Brainfuck] -> [IL]
compile []               = []
compile (Repeat l : bs)  = Loop 0 (compile l) : compile bs
compile (Token tok : bs) = tok' : compile bs
  where
    tok' = case tok of
      Plus       -> Add 0 $ Expr.Const 1
      Minus      -> Add 0 $ Expr.Const (-1)
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar 0
      Input      -> GetChar 0
