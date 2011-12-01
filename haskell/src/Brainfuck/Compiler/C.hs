module Brainfuck.Compiler.C where

import Brainfuck.Compiler.IL

toC :: [IL] -> String
toC []          = []
toC (Loop l:bs) = concat ["while (*ptr) { ", toC l, "} "] ++ toC bs
toC (Poke i:bs) = concat ["*ptr += ", show i, "; "] ++ toC bs
toC (RightShifts i:bs) = concat ["ptr += ", show i, "; "] ++ toC bs
toC (LeftShifts i:bs) = concat ["ptr -= ", show i, "; "] ++ toC bs
toC (PutChar:bs) = "putchar(*ptr); " ++ toC bs
toC (GetChar:bs) = "*ptr = getchar(); " ++ toC bs
