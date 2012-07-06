module Brainfuck.Compiler.Inlining where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Data.Expr
import Brainfuck.Data.IL

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

optimisticInlining :: Int -> Expr -> [IL] -> Maybe [IL]
optimisticInlining d e xs | c1 <= c2 = Nothing
                          | otherwise = Just xs'
  where
    xs' = inline d e xs
    c1  = 1 + exprComplexity e + ilComplexity xs
    c2  = ilComplexity xs'

exprComplexity :: Expr -> Int
exprComplexity = unfold (+) (+) f
  where
    f (Const _) = 0
    f (Get _)   = 1
    f _         = error "unfold Expr error"

ilComplexity :: [IL] -> Int
ilComplexity = foldr ((+) . f) 0
  where
    f x = case x of
      While e ys -> 1 + exprComplexity e + ilComplexity ys
      If e ys    -> 1 + exprComplexity e + ilComplexity ys
      Set _ e    -> 1 + exprComplexity e
      Shift _    -> 1
      PutChar e  -> 1 + exprComplexity e
      GetChar _  -> 1

inline :: Int -> Expr -> [IL] -> [IL]
inline d e []       = [Set d e]
inline d e (x : xs) = case x of
  Set d' e' | d == d'                -> Set d' (ie e') : xs
            | not (exprDepends d' e) -> Set d' (ie e') : inline d e xs
  PutChar e'                         -> PutChar (ie e') : inline d e xs

  _ -> Set d e : x : xs

  where
    ie = inlineExpr d e
