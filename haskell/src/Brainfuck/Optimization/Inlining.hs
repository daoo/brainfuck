{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Inlining where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Optimization.Analysis

data Occurs = GetOnce | SetOnce | InLoop [Occurs] | InIf [Occurs]
  deriving (Show)

occurs :: Int -> AST -> [Occurs]
occurs d = \case
  Nop                  -> []
  Instruction fun next -> case fun of

    Set d' e | d == d'     -> SetOnce : expr e ++ occurs d next
             | otherwise   -> expr e ++ occurs d next
    PutChar e              -> expr e ++ occurs d next
    GetChar d' | d == d'   -> SetOnce : occurs d next
               | otherwise -> occurs d next
    Shift s                -> occurs (d - s) next

  Flow ctrl inner next -> case ctrl of

    Never   -> occurs d next
    Once    -> occurs d inner ++ occurs d next
    Forever -> InLoop (occurs d inner) : occurs d next

    While e -> InLoop (expr e ++ occurs d inner) : occurs d next
    If e    -> expr e ++ InIf (occurs d inner) : occurs d next

  where
    expr = unfold (flip const) (const (++)) (\case
      Get d' | d == d' -> [GetOnce]
      _                -> [])

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

heuristicInlining :: Int -> Expr -> AST -> Maybe AST
heuristicInlining d e xs | c1 <= c2  = Just $ inline d e xs
                         | otherwise = Nothing
  where
    c1 = exprComplexity e
    c2 = allowedComplexity $ occurs d xs

optimisticInlining :: Int -> Expr -> AST -> Maybe AST
optimisticInlining d e xs | c1 <= c2 = Nothing
                          | otherwise = Just xs'
  where
    xs' = inline d e xs
    c1  = 1 + exprComplexity e + ilComplexity xs
    c2  = ilComplexity xs'

exprComplexity :: Expr -> Int
exprComplexity = unfold (flip const) (const (+)) f
  where
    f = \case
      Const _ -> 0
      Get _   -> 1

ilComplexity :: AST -> Int
ilComplexity = \case
  Nop                  -> 0
  Instruction fun next -> function fun + ilComplexity next
  Flow ctrl inner next -> control inner next ctrl

  where
    function = \case
      Set _ e    -> 1 + exprComplexity e
      Shift _    -> 1
      PutChar e  -> 1 + exprComplexity e
      GetChar _  -> 1

    control inner next = \case
      Forever -> ilComplexity inner
      Once    -> ilComplexity inner + ilComplexity next
      Never   -> ilComplexity next

      While e -> 1 + exprComplexity e
      If e    -> 1 + exprComplexity e

inline :: Int -> Expr -> AST -> AST
inline d e = \case
  Nop -> Instruction (Set d e) Nop

  x@(Instruction fun next) -> case fun of

    Set d' e' | d == d'                -> Instruction (Set d' (ie e')) next
              | not (exprDepends d' e) -> Instruction (Set d' (ie e')) (inline d e next)

    PutChar e' -> Instruction (PutChar (ie e')) (inline d e next)

    _ -> Instruction (Set d e) x

  x@(Flow ctrl inner next) -> case ctrl of
    -- Inline into If
    -- Inlining into the condition is safe, we keep the set before the if since
    -- the value has to change even if the condition evaluates to false.
    -- But we also inline into each expression within the if, this can reduce
    -- some += expressions to constant assigns.
    If e' -> Instruction (Set d e) (Flow (If (ie e')) (inline d e inner) next)

    _ -> Instruction (Set d e) x

  where
    ie = inlineExpr d e
