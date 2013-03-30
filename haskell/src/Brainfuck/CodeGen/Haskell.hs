{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Haskell (writeHaskell) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Text.CodeWriter

writeHaskell :: Tarpit -> CodeWriter ()
writeHaskell ast = do
  line "import Brainfuck.Data.Expr"
  line "import Brainfuck.Data.IOMemory"
  line ""
  line "main :: IO ()"
  line "main = do"
  indentedM $ do
    line "m <- newMemory 30001"
    line "program (setMemory m) (putMemory m) (getMemory m) (evalMemory m) (whileMemory m) (ifMemory m)"
  line ""
  line "program set put get eval while when = runMemory $ do"
  indentedM $ go ast

  where
    go = \case
      Nop                  -> return ()
      Instruction fun next -> lineM (function fun) >> go next
      Flow ctrl inner next -> control ctrl >> indentedM (go inner) >> go next

    control = \case
      Forever -> block "while" (constant 1)
      While e -> block "while" e
      Once    -> block "when" (constant 1)
      Never   -> block "when" (constant 0)
      If e    -> block "when" e

    function = \case
      Assign d e -> string "set " >> int d >> string " $ " >> string (show e)
      PutChar e  -> string "put $ " >> string (show e)
      GetChar d  -> string "get " >> int d
      Shift d    -> string "shift (" >> int d >> string ")"

    block str e = lineM $ do
      string str
      string " ("
      string $ show e
      string ") $ do"
