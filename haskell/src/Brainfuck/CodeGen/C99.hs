{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.C99 where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Monad
import Text.CodeWriter

writeExpr :: Expr -> CodeWriter ()
writeExpr = \case
  Const c           -> int c
  Var n d (Const 0) -> mult n d
  Var n d xs        -> mult n d >> string " + " >> writeExpr xs
  where
    mult 1 d = string "ptr[" >> int d >> string "]"
    mult n d = int n >> string " * ptr[" >> int d >> string "]"

writeC99 :: Tarpit -> CodeWriter ()
writeC99 code = do
  line "#include <stdio.h>"
  line ""
  line "int main() {"
  indentedM $ do
    when (usesMemory code) $ do
      line "unsigned char mem[30001];"
      line "unsigned char* ptr = mem;"

    go code

    line "return 0;"
  line "}"
  where
    go :: Tarpit -> CodeWriter ()
    go = \case
      Nop                  -> return ()
      Instruction fun next -> lineM (function fun >> string ";") >> go next
      Flow ctrl inner next -> control ctrl inner >> go next

    control = \case
      Forever -> block "while" (Const 1)
      While e -> block "while" e
      Once    -> block "if" (Const 1)
      Never   -> block "if" (Const 0)
      If e    -> block "if" e

    block :: String -> Expr -> Tarpit -> CodeWriter ()
    block word e ys = do
      lineM $ do
        string word
        string " ("
        writeExpr e
        string ") {"
      indentedM $ go ys
      line "}"

    function = \case
      Assign d e -> ptr d (writeExpr e)

      Shift s   -> string "ptr += " >> int s
      PutChar e -> string "putchar(" >> writeExpr e >> string ")"
      GetChar d -> ptr d (string "getchar()")

    ptr :: Int -> CodeWriter () -> CodeWriter ()
    ptr diff value = do
      string "ptr["
      int diff
      string "] = "
      value
