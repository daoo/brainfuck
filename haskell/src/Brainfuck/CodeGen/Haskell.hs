{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Haskell (writeHaskell) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Text.CodeWriter

writeHaskell :: Tarpit -> CodeWriter ()
writeHaskell code = do
  line "import Brainfuck.Data.Expr hiding (eval)"
  line "import Brainfuck.Data.IOMemory"
  line "import Control.Monad hiding (when)"
  line "import Control.Monad.Trans"
  line "import Data.Char"
  line "import Prelude hiding (read)"
  line ""
  line "put = lift . putChar . chr"
  line "get = ord `fmap` getChar"
  line ""
  line "main :: IO ()"
  line "main = runMemory 30001 $ do"
  indentedM $ go code

  where
    go = \case
      Nop                  -> return ()
      Instruction fun next -> lineM (function fun) >> go next
      Flow ctrl inner next -> control ctrl >> indentedM (go inner) >> go next

    control = \case
      If e    -> block "when" e
      While e -> block "while" e

    function = \case
      Assign d e -> string "write " >> safeint d >> string " " >> expr e
      PutChar e  -> string "put " >> expr e
      GetChar d  -> string "write " >> safeint d >> string " =<< get"
      Shift d    -> string "shift " >> safeint d

    safeint d = surround '(' ')' (d < 0) (int d)

    expr (Const c) = int c
    expr e         = string "=<< eval (" >> string (shows e ")")

    block str e = lineM $ do
      string str
      string " ("
      string $ show e
      string ") $ do"
