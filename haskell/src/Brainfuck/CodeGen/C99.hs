{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.C99 where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Optimization.Analysis
import Control.Monad
import Data.Char
import Text.CodeWriter

showExpr :: Expr -> ShowS
showExpr = \case
  Value v              -> value v
  OperateUnary op a    -> unary op a
  OperateBinary op a b -> binary op a b

  where
    unary op a = case op of
      Id     -> showExpr a
      Negate -> showString "-" . showParen True (showExpr a)

    binary op a b = case op of
      Add -> showExpr a . showString " + " . showExpr b
      Mul -> showParen (paren Mul a) (showExpr a) . showString " * " . showParen (paren Mul b) (showExpr b)

    value = \case
      Const c -> shows c
      Get d   -> showString "ptr[" . shows d . showString "]"

    paren Mul = \case
      OperateBinary Add _ _ -> True
      OperateBinary Mul _ _ -> False
      OperateUnary Id a     -> paren Mul a
      OperateUnary Negate _ -> True
      Value _               -> False

    paren Add = \case
      OperateUnary Negate _ -> True
      OperateUnary Id a     -> paren Add a
      _                     -> False

showAST :: AST -> String
showAST ast = writeCode $ do
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
    go :: AST -> CodeWriter ()
    go = \case
      Nop -> return ()
      Instruction fun next -> lineM (function fun >> string ";") >> go next
      Flow ctrl inner next -> control ctrl inner >> go next

    control = \case
      Forever -> block "while" (mkInt 1)
      While e -> block "while" e
      Once    -> block "if" (mkInt 1)
      Never   -> block "if" (mkInt 0)
      If e    -> block "if" e

    block :: String -> Expr -> AST -> CodeWriter ()
    block word e ys = do
      lineM $ string word >> string " (" >> (string $ showExpr e ") {")
      indentedM $ go ys
      line "}"

    function x = case x of
      Set d e -> ptr d "=" (showExpr e "")

      PutChar (Value (Const c)) -> string "putchar(" >> string (show $ chr c) >> string ")"

      Shift s   -> string "ptr += " >> string (show s)
      PutChar e -> string "putchar(" >> string (showExpr e ")")
      GetChar p -> ptr p "=" "getchar()"

    ptr :: Int -> String -> String -> CodeWriter ()
    ptr d op b = do
      string "ptr["
      string $ show d
      string "] "
      string op
      string " "
      string b
