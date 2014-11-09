{-# LANGUAGE LambdaCase #-}
module Properties.Expressions
  ( propExprIsSorted
  , propExprAddSorted
  , propExprEvalAdd
  , propExprEvalMul
  ) where

import Brainfuck.Data.Expr

propExprIsSorted :: Expr -> Bool
propExprIsSorted = go
  where
    go = \case
      Const _                -> True
      Var _ _ (Const _)      -> True
      Var _ d1 (Var _ d2 xs) -> d1 <= d2 && go xs

propExprAddSorted :: Expr -> Expr -> Bool
propExprAddSorted a b = propExprIsSorted (a .+ b)

propExprEvalAdd :: [Int] -> Expr -> Expr -> Bool
propExprEvalAdd xs a b = evalExpr (mem xs) (a .+ b) == evalExpr (mem xs) a + evalExpr (mem xs) b

propExprEvalMul :: [Int] -> Int -> Expr -> Bool
propExprEvalMul xs i e = evalExpr (mem xs) (i .* e) == i * evalExpr (mem xs) e

mem :: [Int] -> Int -> Int
mem [] _ = 0
mem xs i = xs !! (i `mod` length xs)
