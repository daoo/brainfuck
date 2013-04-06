{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.C99 where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Monad
import Text.CodeWriter

writeExpr :: Expr -> CodeWriter ()
writeExpr = \case
  Expr c [] -> int c
  Expr 0 v  -> go v
  Expr c v  -> int c >> string " + " >> go v
  where
    go = \case
      []   -> return ()
      [x]  -> mult x
      x:xs -> mult x >> string " + " >> go xs

    mult (Mult 1, Var d) = string "ptr[" >> int d >> string "]"
    mult ((Mult n), Var d) = int n >> string " * ptr[" >> int d >> string "]"

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
      Forever -> block "while" (constant 1)
      While e -> block "while" e
      Once    -> block "if" (constant 1)
      Never   -> block "if" (constant 0)
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
