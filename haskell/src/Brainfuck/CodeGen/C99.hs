{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.C99
  ( writeC99
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Monad
import Data.Char
import Text.CodeWriter

runtime :: String
runtime = "#include <stdio.h>\n"

memory :: String
memory =
  "unsigned char mem[30001];\n\
  \unsigned char* ptr = mem;\n"

writeExpr :: IntExpr -> CodeWriter ()
writeExpr = \case
  Const c           -> int c
  Var n d (Const 0) -> mult n d
  Var n d xs        -> mult n d >> string " + " >> writeExpr xs
  where
    mult 1 d = string "ptr[" >> int d >> char ']'
    mult n d = int n >> string " * ptr[" >> int d >> char ']'

writeC99 :: Tarpit -> CodeWriter ()
writeC99 code = do
  string runtime
  newline
  line "int main() {"
  indented $ do
    unless (putConstOnly code) $ string memory
    go code
    line "return 0;"
  line "}"
  where
    go :: Tarpit -> CodeWriter ()
    go = \case
      Nop                  -> return ()
      Instruction fun next -> lined (function fun >> char ';') >> go next
      Flow ctrl inner next -> control ctrl inner >> go next

    control = \case
      While e -> block "while" e
      If e    -> block "if" e

    block word e ys = do
      lined $ do
        string word
        string " ("
        writeExpr e
        string ") {"
      indented $ go ys
      line "}"

    function = \case
      Shift s    -> string "ptr" .+= int s
      GetChar d  -> ptr d .= string "getchar()"

      Assign d (Var 1 d' (Const c)) | d == d' -> ptr d .+= int c
      Assign d e                              -> ptr d .= writeExpr e

      PutChar (Const c) -> putchar $ string $ show (chr c)
      PutChar e         -> putchar $ writeExpr e

    putchar a = string "putchar(" >> a >> char ')'
    ptr d = string "ptr[" >> int d >> char ']'

    a .=  b = a >> string " = " >> b
    a .+= b = a >> string " += " >> b
