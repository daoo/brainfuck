module Brainfuck.Compiler.C (showC) where

import Brainfuck.Compiler.IL

showC :: [IL] -> String
showC = program 30000 . toC 1

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
indent i = take (2 * i) $ repeat ' '

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
    helper i (x:xs)      = concat [ indent i
                                  , line x
                                  , "\n"
                                  , helper i xs ]

    line (Poke i)        = concat [ "*ptr += ", show i, ";" ]
    line (RightShifts i) = concat [ "ptr += ", show i, ";" ]
    line (LeftShifts i)  = concat [ "ptr -= ", show i, ";" ]
    line PutChar         = "putchar(*ptr);"
    line GetChar         = "*ptr = getchar();"
    line _               = error "error"
