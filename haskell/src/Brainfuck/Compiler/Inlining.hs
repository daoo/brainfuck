module Brainfuck.Compiler.Inlining where

import Brainfuck.Compiler.Analyzer
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

heursticInlining :: Int -> Expr -> [IL] -> Maybe [IL]
heursticInlining d e xs =
  let xs' = inline d e xs
      m   = 1 + complexity e + measure xs
      m'  = measure xs'
   in if m < m' then Nothing else Just xs'

measure :: [IL] -> Int
measure = foldr ((+) . f) 0
  where
    f x = case x of
      While e ys -> 1 + complexity e + measure ys
      If e ys    -> 1 + complexity e + measure ys
      Set _ e    -> 1 + complexity e
      Shift _    -> 1
      PutChar e  -> 1 + complexity e
      GetChar _  -> 1

inline :: Int -> Expr -> [IL] -> [IL]
inline d e []       = [Set d e]
inline d e (x : xs) = if inlineValid d e x
  then case x of
    Set d' e' -> Set d' (inlineExpr d e e') : inline d e xs
    _         -> undefined

  else Set d e : x : xs

inlineValid :: Int -> Expr -> IL -> Bool
inlineValid d e x = case x of
  Set d' e' -> d /= d' && not (exprDepends d e') && not (exprDepends d' e)
  _         -> False
