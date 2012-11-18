{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compiler.Target.C99 (showC) where

import Brainfuck.Compiler.Analysis
import Brainfuck.Data.Expr
import Brainfuck.Data.IL
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

showC :: [IL] -> String
showC ils = writeCode $ do
  line "#include <stdio.h>"
  line ""
  line "int main() {"
  indentedM $ do
    when (usesMemory ils) $ do
      line "unsigned char mem[30001];"
      line "unsigned char* ptr = mem;"

    code ils

    line "return 0;"
  line "}"
  where
    code :: [IL] -> CodeWriter
    code []       = return ()
    code (x : xs) = case x of
      While e ys -> block "while" e ys >> code xs
      If e ys    -> block "if" e ys >> code xs
      _          -> lineM (statement x >> string ";") >> code xs

    block :: String -> Expr -> [IL] -> CodeWriter
    block word e ys = do
      line $ shows word $ showString " (" $ showExpr e ") {"
      indentedM $ code ys
      line "}"

    statement x = case x of
      Set d e -> case removeGet d e of
        Nothing -> ptr d "=" (showExpr e "")
        Just e' -> ptr d "+=" (showExpr e' "")

      PutChar (Const c) -> string "putchar(" >> string (show $ chr c) >> string ")"

      Shift s   -> string "ptr += " >> string (show s)
      PutChar e -> string "putchar(" >> string (showExpr e ")")
      GetChar p -> ptr p "=" "getchar()"

      While _ _ -> error "While can not be composed into a single line"
      If _ _    -> error "If can not be composed into a single line"

    ptr :: Int -> String -> String -> CodeWriter
    ptr d op b = do
      string "ptr["
      string $ show d
      string "] "
      string op
      string " "
      string b
