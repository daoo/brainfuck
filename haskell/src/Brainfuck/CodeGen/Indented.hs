{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Indented where

import Brainfuck.Data.AST
import Text.CodeWriter

showAST :: AST -> String
showAST = writeCode . go
  where
    go = \case
      Nop                  -> return ()
      Instruction fun next -> line (show fun) >> go next
      Flow ctrl inner next -> block (show ctrl) inner >> go next

    block str inner = do
      line str
      incIndent
      go inner
      decIndent
