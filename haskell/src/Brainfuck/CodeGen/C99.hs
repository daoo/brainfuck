{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Brainfuck.CodeGen.C99
  ( writeC99
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Monad
import Data.ByteString (ByteString)
import Data.Char
import Text.CodeWriter

runtime :: ByteString
runtime = "#include <stdio.h>\n"

writeExpr :: Expr -> CodeWriter ()
writeExpr = buildExpr plus variable int
  where
    plus a b = a >> string " + " >> b

    variable 1 x = string "ptr[" >> int x >> char ']'
    variable a x = int a >> string " * ptr[" >> int x >> char ']'

writeC99 :: Tarpit -> CodeWriter ()
writeC99 code = do
  string runtime
  newline
  line "int main()"
  line "{"
  indented $ do
    unless (putConstOnly code) $ do
      line "unsigned char mem[30001];"
      line "unsigned char* ptr = mem;"
    writeStms code
    line "return 0;"
  line "}"

writeStms :: Tarpit -> CodeWriter ()
writeStms = \case
  Nop                  -> return ()
  Instruction fun next -> lined (function fun >> char ';') >> writeStms next
  Flow ctrl inner next -> control ctrl inner >> writeStms next

  where

    control = \case
      While e -> block "while" e
      If e    -> block "if" e

    block word e ys = do
      lined $ do
        string word
        string " ("
        writeExpr e
        string ") {"
      indented $ writeStms ys
      line "}"

    function = \case
      Shift s   -> string "ptr" .+= int s
      GetChar d -> ptr d .= string "getchar()"

      Assign d (Add d' c) | d == d' -> ptr d .+= int c
      Assign d e                   -> ptr d .= writeExpr e

      PutChar (Constant c) -> putchar $ writeChar c
      PutChar e            -> putchar $ writeExpr e

    putchar a = string "putchar(" >> a >> char ')'
    ptr d = string "ptr[" >> int d >> char ']'

    a .=  b = a >> string " = " >> b
    a .+= b = a >> string " += " >> b

    writeChar = surround True '\'' '\'' . \case
      0x07 -> char '\\' >> char 'a'
      0x08 -> char '\\' >> char 'b'
      0x0C -> char '\\' >> char 'f'
      0x0A -> char '\\' >> char 'n'
      0x0D -> char '\\' >> char 'r'
      0x09 -> char '\\' >> char 't'
      0x0B -> char '\\' >> char 'v'
      0x5C -> char '\\' >> char '\\'
      0x27 -> char '\\' >> char '\''
      c    -> char (chr c)
