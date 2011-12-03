module Brainfuck.Compiler.C (showC) where

import Brainfuck.Compiler.IL

showC :: [IL] -> String
showC = program . toC

includes     = unlines ["#include <stdio.h>", "#include <stdlib.h>"]
main code    = unlines ["int main() {", code, "return 0;", "}"]
program code = unlines [includes, main code]

toC :: [IL] -> String
toC []                  = []
toC (Loop l:bs)         = concat ["while (*ptr) { ", toC l, "} ", toC bs]
toC (Poke i:bs)         = concat ["*ptr += ", show i, "; ", toC bs]
toC (RightShifts i:bs)  = concat ["ptr += ", show i, "; ", toC bs]
toC (LeftShifts i:bs)   = concat ["ptr -= ", show i, "; ", toC bs]
toC (PutChar:bs)        = "putchar(*ptr); " ++ toC bs
toC (GetChar:bs)        = "*ptr = getchar(); " ++ toC bs
