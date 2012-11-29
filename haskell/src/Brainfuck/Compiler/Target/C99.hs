{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compiler.Target.C99 (showC) where

import Brainfuck.Compiler.Analysis
import Brainfuck.Data.Expr
import Brainfuck.Data.AST
import Control.Monad
import Data.Char
import Text.CodeWriter

showExpr :: Expr -> ShowS
showExpr = \case
  Const c         -> shows c
  Get d           -> showString "ptr[" . shows d . showString "]"
  Add a b         -> showExpr a . showString " + " . showExpr b
  Mul (Add a b) c -> showString "(" . showExpr a . showString " + " . showExpr b . showString ") * " . showExpr c
  Mul a (Add b c) -> showExpr a . showString " * (" . showExpr b . showString " + " . showExpr c . showString ")"
  Mul a b         -> showExpr a . showString " * " . showExpr b

removeGet :: Int -> Expr -> Maybe Expr
removeGet d = \case
  Add a b | h a       -> Just b
          | h b       -> Just a
          | otherwise -> case (removeGet d a, removeGet d b) of
                           (Nothing, Nothing) -> Nothing
                           (Just a', Just b') -> Just $ a' `Add` b'
                           (Just a', Nothing) -> Just $ a' `Add` b
                           (Nothing, Just b') -> Just $ a `Add` b'

  _ -> Nothing
  where
    h = \case
      Get d' -> d == d'
      _      -> False

showC :: AST -> String
showC ast = writeCode $ do
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
    go :: AST -> CodeWriter
    go = \case
      Nop -> return ()
      Instruction fun next -> lineM (function fun >> string ";") >> go next
      Flow ctrl inner next -> control ctrl inner >> go next

    control = \case
      Forever -> block "while" (Const 1)
      While e -> block "while" e
      Once    -> block "if" (Const 1)
      Never   -> block "if" (Const 0)
      If e    -> block "if" e

    block :: String -> Expr -> AST -> CodeWriter
    block word e ys = do
      lineM $ string word >> string " (" >> (string $ showExpr e ") {")
      indentedM $ go ys
      line "}"

    function x = case x of
      Set d e -> case removeGet d e of
        Nothing -> ptr d "=" (showExpr e "")
        Just e' -> ptr d "+=" (showExpr e' "")

      PutChar (Const c) -> string "putchar(" >> string (show $ chr c) >> string ")"

      Shift s   -> string "ptr += " >> string (show s)
      PutChar e -> string "putchar(" >> string (showExpr e ")")
      GetChar p -> ptr p "=" "getchar()"

    ptr :: Int -> String -> String -> CodeWriter
    ptr d op b = do
      string "ptr["
      string $ show d
      string "] "
      string op
      string " "
      string b
