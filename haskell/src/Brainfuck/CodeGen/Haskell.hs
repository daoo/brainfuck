{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Brainfuck.CodeGen.Haskell
  ( writeHaskell
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Text.CodeWriter

writeHaskell :: Tarpit -> CodeWriter ()
writeHaskell code = do
  line "import Brainfuck.Data.Expr"
  line "import Brainfuck.Data.IOMachine"
  line ""
  line "main :: IO ()"
  line "main = runMemory 30001 $ do"
  indented $ go code

  where
    go = \case
      Nop                  -> return ()
      Instruction fun next -> lined (function fun) >> go next
      Flow ctrl inner next -> control ctrl >> indented (go inner) >> go next

    control = \case
      If e    -> block "when" e
      While e -> block "while" e

    function = \case
      Assign d e -> string "set "   >> safeint d >> string " $ " >> expr e
      PutChar e  -> string "put $ " >> expr e
      GetChar d  -> string "get "   >> safeint d
      Shift d    -> string "shift " >> safeint d

    block str e = lined $ do
      string str
      string " ("
      expr e
      string ") $ do"

    expr = buildExpr plus variable constant

    plus a b   = a >> char ' ' >> parentheses b
    constant n = string "Const " >> safeint n

    variable n v = do
      string "Var "
      safeint n
      char ' '
      safeint v

    safeint d = surround (d < 0) '(' ')' (int d)
