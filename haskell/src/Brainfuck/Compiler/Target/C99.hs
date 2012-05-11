module Brainfuck.Compiler.Target.C99 (showC, optimizeForC) where

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . times optimize 100
  where
    optimize = id
             . mapIL optimizeExpressions
             . reduceLoops
             . mapIL optimizeExpressions
             . filterIL clean
             . mapIL optimizeExpressions
             . applyIL

showC :: [IL] -> String
showC ils = program mem $ toC 1 ils
  where
    mem = 30001

showExpr :: Expr -> String
showExpr (Const c)            = show c
showExpr (Get d)              = "ptr[" ++ show d ++ "]"
showExpr (Add e1 e2)          = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul (Add e1 e2) e3) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ") * " ++ showExpr e3
showExpr (Mul e1 (Add e2 e3)) = showExpr e1 ++ " * (" ++ showExpr e2 ++ " + " ++ showExpr e3 ++ ")"
showExpr (Mul e1 e2)          = showExpr e1 ++ " * " ++ showExpr e2

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
    helper _ []              = []
    helper i (Loop d l : xs) = concat [ indent i
                                      , "while (ptr["
                                      , show d
                                      , "]) {\n"
                                      , helper (i + 1) l
                                      , indent i
                                      , "}\n"
                                      , helper i xs ]
    helper i (x : xs) = concat [ indent i
                               , line x
                               , "\n"
                               , helper i xs ]

    line (Set d1 (Get d2 `Add` Const c)) | d1 == d2 = concat [ "ptr[", show d1, "] += ", show c, ";" ]
    line (Set d1 (Const c `Add` Get d2)) | d1 == d2 = concat [ "ptr[", show d1, "] += ", show c, ";" ]

    line (Set d e)   = concat [ "ptr[", show d, "] = ", showExpr e, ";" ]
    line (Shift s)   = concat [ "ptr += ", show s, ";" ]
    line (PutChar e) = concat [ "putchar(", showExpr e, ");" ]
    line (GetChar p) = concat [ "ptr[", show p, "] = getchar();" ]

    line (Loop _ _) = error "error"
