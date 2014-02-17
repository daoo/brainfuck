{-# LANGUAGE LambdaCase #-}
module Properties.Expressions
  ( propExprIsSorted
  , propExprAddSorted
  , propExprEvalAdd
  , propExprEvalMul
  ) where

import Brainfuck.Data.Expr

propExprIsSorted :: Expr Int Int -> Bool
propExprIsSorted = go True
  where
    go b = \case
      Const _                -> b
      Var _ _ (Const _)      -> b
      Var _ d1 (Var _ d2 xs) -> go (b && d1 <= d2) xs

propExprAddSorted :: Expr Int Int -> Expr Int Int -> Bool
propExprAddSorted a b = propExprIsSorted (a .+ b)

propExprEvalAdd :: Expr Int Int -> Expr Int Int -> Bool
propExprEvalAdd a b = evalExpr id (a .+ b) == evalExpr id a + evalExpr id b

propExprEvalMul :: Int -> Expr Int Int -> Bool
propExprEvalMul i e = evalExpr id (i .* e) == i * evalExpr id e
