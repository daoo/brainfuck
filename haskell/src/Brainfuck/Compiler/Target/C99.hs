module Brainfuck.Compiler.Target.C99 (showC, optimizeForC) where

import Control.Monad.Writer
import Control.Monad.State

import Data.Char

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimize
import Brainfuck.Ext

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . whileModified pipeline
  where
    pipeline = pipe
      [ mapIL optimizeExpressions
      , inlineZeros
      , whileToIf
      , reduceCopyLoops
      , filterIL clean
      , inlining
      , moveShifts
      , mergeKind
      ]

showExpr :: Expr -> ShowS
showExpr (Const c)            = shows c
showExpr (Get d)              = showString "ptr[" . shows d . showString "]"
showExpr (Add e1 e2)          = showExpr e1 . showString " + " . showExpr e2
showExpr (Mul (Add e1 e2) e3) = showString "(" . showExpr e1 . showString " + " . showExpr e2 . showString ") * " . showExpr e3
showExpr (Mul e1 (Add e2 e3)) = showExpr e1 . showString " * (" . showExpr e2 . showString " + " . showExpr e3 . showString ")"
showExpr (Mul e1 e2)          = showExpr e1 . showString " * " . showExpr e2

type CodeWriter = StateT Int (Writer String) ()

indent :: Int -> String
indent i = replicate (2 * i) ' '

line :: String -> CodeWriter
line str = do
  i <- get
  tell $ indent i
  tell str
  tell "\n"

lineM :: CodeWriter -> CodeWriter
lineM f = do
  i <- get
  tell $ indent i
  f
  tell "\n"

incIndent :: CodeWriter
incIndent = modify (+1)

decIndent :: CodeWriter
decIndent = modify (subtract 1)

showC :: [IL] -> String
showC ils = execWriter $ execStateT (go ils) 0
  where
    go :: [IL] -> CodeWriter
    go ils' = do
      line "#include <stdio.h>"
      line ""
      line "int main() {"
      incIndent
      when (usesMemory ils') $ do
        line $ "unsigned char mem[" ++ show defaultMem ++ "];"
        line "unsigned char* ptr = mem;"

      code ils'

      line "return 0;"
      decIndent
      line "}"

    defaultMem :: Int
    defaultMem = 30001

    code :: [IL] -> CodeWriter
    code []       = return ()
    code (x : xs) = case x of
      While e ys -> block "while" e ys >> code xs
      If e ys    -> block "if" e ys >> code xs
      _          -> lineM (asdf x >> tell ";") >> code xs

    block :: String -> Expr -> [IL] -> CodeWriter
    block word e ys = do
      lineM $ do
        tell word
        tell " ("
        tell $ showExpr e ") {"
      incIndent
      code ys
      decIndent
      line "}"

    asdf x = case x of
      Set d1 (Get d2 `Add` Const c) | d1 == d2 -> ptr d1 "+=" (show c)
      Set d1 (Const c `Add` Get d2) | d1 == d2 -> ptr d1 "+=" (show c)

      PutChar (Const c) -> tell "putchar('" >> tell (show $ chr c) >> tell "')"

      Set d e   -> ptr d "=" (showExpr e "")
      Shift s   -> tell "ptr += " >> tell (show s)
      PutChar e -> tell "putchar(" >> tell (showExpr e ")")
      GetChar p -> ptr p "=" "getchar()"

      While _ _ -> error "While can not be composed into a single line"
      If _ _    -> error "If can not be composed into a single line"

    ptr :: Int -> String -> String -> CodeWriter
    ptr d op b = do
      tell "ptr["
      tell $ show d
      tell "] "
      tell op
      tell " "
      tell b
