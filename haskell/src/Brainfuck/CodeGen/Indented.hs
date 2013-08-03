{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Indented
  ( writeIndented
  ) where

import Brainfuck.Data.Tarpit
import Text.CodeWriter

writeIndented :: Tarpit -> CodeWriter ()
writeIndented = \case
  Nop                  -> return ()
  Instruction fun next -> line (show fun) >> writeIndented next
  Flow ctrl inner next -> block (show ctrl) inner >> writeIndented next
  where
    block str inner = do
      line str
      incIndent
      writeIndented inner
      decIndent
