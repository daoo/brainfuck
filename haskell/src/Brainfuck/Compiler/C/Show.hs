module Brainfuck.Compiler.C.Show (showC) where

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
    helper i (Loop d l : xs) = concat [ indent i
                                      , "while (*(ptr + "
                                      , show d
                                      , ")) {\n"
                                      , helper (i + 1) l
                                      , indent i
                                      , "}\n"
                                      , helper i xs ]
    helper i (x : xs) = concat [ indent i
                               , line x
                               , "\n"
                               , helper i xs ]

    line (AddFrom d1 d2) = concat [ "ptr[", show d1, "] += ptr[", show d2, "];" ]
    line (SetFrom d1 d2) = concat [ "ptr[", show d1, "] = ptr[", show d2, "];" ]
    line (Poke p i)      = concat [ "ptr[", show p, "] += ", show i, ";" ]
    line (Set p i)       = concat [ "ptr[", show p, "] = ", show i, ";" ]
    line (Shift s)       = concat [ "ptr += ", show s, ";" ]
    line (PutChar p)     = concat [ "putchar(ptr[", show p, "]);" ]
    line (GetChar p)     = concat [ "ptr[", show p, "] = getchar();" ]

    line (Loop _ _) = error "error"
