module Brainfuck.Compiler.Target.C99 (showC, optimizeForC) where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimize
import Brainfuck.Ext

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . times optimize 100
  where
    optimize = id
             . inlineZeros
             . mapIL optimizeExpressions
             . reduceLoops
             . mapIL optimizeExpressions
             . filterIL clean
             . mapIL optimizeExpressions
             . applyIL

showExpr :: Expr -> ShowS
showExpr (Const c)            = shows c
showExpr (Get d)              = showString "ptr[" . shows d . showString "]"
showExpr (Add e1 e2)          = showExpr e1 . showString " + " . showExpr e2
showExpr (Mul (Add e1 e2) e3) = showString "(" . showExpr e1 . showString " + " . showExpr e2 . showString ") * " . showExpr e3
showExpr (Mul e1 (Add e2 e3)) = showExpr e1 . showString " * (" . showExpr e2 . showString " + " . showExpr e3 . showString ")"
showExpr (Mul e1 e2)          = showExpr e1 . showString " * " . showExpr e2

indent :: Int -> ShowS
indent i = (replicate (2 * i) ' ' ++)

showC :: [IL] -> String
showC ils = unlines $ begin $ mem ils $ code 1 ils $ newLine end
  where
    defaultMem :: Int
    defaultMem = 30001

    newLine :: [String] -> [String]
    newLine = ("" :)

    begin :: [String] -> [String]
    begin prep = "#include <stdio.h>" : ("" : ("int main() {" : prep))

    end :: [String]
    end = [ indent 1 "return 0;", "}" ]
          
    mem xs prep = if usesMemory xs
      then alloc $ ptr $ newLine prep
      else prep
      where
        alloc = (:) (indent 1 $ showString "char mem[" $ shows defaultMem "];")
        ptr   = (:) (indent 1 "char* ptr = mem;")

    code _ [] prep                 = prep
    code i (Loop d loop : xs) prep = while $ body $ close $ code i xs prep
      where
        while = (:) (indent i $ showString "while (ptr[" $ shows d "]) {")
        close = (:) (indent i "}")
        body  = code (i + 1) loop

    code i (x : xs) prep = indent i (line x) : code i xs prep
      
    line x = case x of
      Set d1 (Get d2 `Add` Const c) | d1 == d2 -> ptr (shows d1) "+=" (shows c)
      Set d1 (Const c `Add` Get d2) | d1 == d2 -> ptr (shows d1) "+=" (shows c)

      Set d e   -> ptr (shows d) "=" (showExpr e)
      Shift s   -> showString "ptr += " $ shows s ";"
      PutChar e -> showString "putchar(" $ showExpr e ");"
      GetChar p -> ptr (shows p) "=" (showString "getchar()")

      Loop _ _ -> error "Should not happen"

      where
        ptr a op b = showString "ptr[" $ a $ showString "] " $ showString op $ showString " " $ b ";"
