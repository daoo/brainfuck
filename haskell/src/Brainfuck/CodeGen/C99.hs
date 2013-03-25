{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.C99 where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Monad
import Data.Char
import Text.CodeWriter

showExpr :: Expr -> ShowS
showExpr = \case
  Const i -> shows i
  Var d   -> showString "ptr[" . shows d . showString "]"
  Add a b -> showExpr a . showString " + " . showExpr b
  Mul a b -> shows a . showString " * " . showParen (paren b) (showExpr b)

  where
    paren = \case
      Add _ _ -> True
      _       -> False

showTarpit :: Tarpit -> String
showTarpit ast = writeCode $ do
  line "#include <stdio.h>"
  line ""
  line "int main() {"
  indentedM $ do
    when (usesMemory ast) $ do
      line "unsigned char mem[30001];"
      line "unsigned char* ptr = mem;"

    go ast

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
      line $ showString word $ showString " (" $ showExpr e ") {"
      indentedM $ go ys
      line "}"

    function = \case
      Assign d e -> ptr d "=" (showExpr e "")

      PutChar (Const c) -> string $ showString "putchar(" $ shows (chr c) ")"

      Shift s   -> string $ showString "ptr += " $ show s
      PutChar e -> string $ showString "putchar(" $ showExpr e ")"
      GetChar p -> ptr p "=" "getchar()"

    ptr :: Int -> String -> String -> CodeWriter ()
    ptr d op b = do
      string "ptr["
      string $ show d
      string "] "
      string op
      string " "
      string b
