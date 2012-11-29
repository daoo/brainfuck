{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compiler.Target.Indented (showIndented) where

import Brainfuck.Data.AST
import Text.CodeWriter

showIndented :: AST -> String
showIndented = writeCode . go
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
