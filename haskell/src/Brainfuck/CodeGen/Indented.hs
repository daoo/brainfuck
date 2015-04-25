{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Indented
  ( writeIndented
  ) where

import Brainfuck.Data.Tarpit
import Data.ByteString.Char8 (pack)
import Text.CodeWriter

writeIndented :: Tarpit -> CodeWriter ()
writeIndented = \case
  Nop                  -> return ()
  Instruction fun next -> line (pack $ show fun) >> writeIndented next
  Flow ctrl inner next -> block (pack $ show ctrl) inner >> writeIndented next
  where
    block str inner = do
      line str
      incIndent
      writeIndented inner
      decIndent
