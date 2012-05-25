module Brainfuck.Compiler.Inlining where

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL

data Occurs = GetOnce | SetOnce | InLoop [Occurs] | InIf [Occurs]
  deriving (Show)

occurs :: Int -> [IL] -> [Occurs]
occurs _ []       = []
occurs d (x : xs) = case x of
  While e ys -> InLoop (occursExpr e ++ occurs d ys) : occurs d xs
  If e ys    -> occursExpr e ++ InIf (occurs d ys) : occurs d xs

  Set d' e | d == d'     -> SetOnce : occursExpr e ++ occurs d xs
           | otherwise   -> occursExpr e ++ occurs d xs
  PutChar e              -> occursExpr e ++ occurs d xs
  GetChar d' | d == d'   -> SetOnce : occurs d xs
             | otherwise -> occurs d xs
  Shift s                -> occurs (d - s) xs

  where
    occursExpr = unfold (++) (++) f

    f (Get d') | d == d' = [GetOnce]
    f _                  = []

inline :: Int -> Expr -> [IL] -> [IL]
inline = undefined

allowedComplexity :: [Occurs] -> Int
allowedComplexity [] = 0
allowedComplexity oc = getCount oc + 2 * setCount oc
  where
    setCount []             = 0
    setCount (InLoop _ : _) = 0
    setCount (InIf ys : xs) = setCount ys + setCount xs
    setCount (SetOnce : xs) = 1 + setCount xs
    setCount (_ : xs)       = setCount xs

    getCount []             = 0
    getCount (InLoop _ : _) = 0
    getCount (InIf ys : xs) = getCount ys + getCount xs
    getCount (GetOnce : xs) = 1 + getCount xs
    getCount (_ : xs)       = getCount xs
