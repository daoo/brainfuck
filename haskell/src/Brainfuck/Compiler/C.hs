module Brainfuck.Compiler.C (showC) where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.IL

showC :: [IL] -> String
showC ils = program mem $ toC 1 ils
  where
    mem = 30001

program :: Int -> String -> String
program mem code =
  unlines [ "#include <stdio.h>"
          , "#include <stdlib.h>"
          , ""
          , "int main() {"
          , indent 1 ++ "char mem[" ++ show mem ++ "];"
          , indent 1 ++ "char *ptr = mem;"
          , ""
          , code
          , "  return 0;"
          , "}" ]

indent :: Int -> String
indent i = replicate (2 * i) ' '

toC :: Int -> [IL] -> String
toC = helper
  where
    helper _ []          = []
    helper i (Loop l:xs) = concat [ indent i
                                  , "while (*ptr) {\n"
                                  , helper (i + 1) l
                                  , indent i
                                  , "}\n"
                                  , helper i xs ]
    helper i (x:xs) = concat [ indent i
                             , line x
                             , "\n"
                             , helper i xs ]

    line (Poke p i)              = concat [ "ptr[", show p, "] += ", show i, ";" ]
    line (Shift (ShiftLeft i))   = concat [ "ptr -= ", show i, ";" ]
    line (Shift (ShiftRight i))  = concat [ "ptr += ", show i, ";" ]
    line (PutChar p)             = concat [ "putchar(ptr[", show p, "]);" ]
    line (GetChar p)             = concat [ "ptr[", show p, "] = getchar();" ]
    line _                       = error "error"