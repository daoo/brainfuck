{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr

propExprIsSorted :: Expr -> Bool
propExprIsSorted = go True
  where
    go b = \case
      Const _                -> b
      Var _ _ (Const _)      -> b
      Var _ d1 (Var _ d2 xs) -> go (b && d1 <= d2) xs

propExprAddSorted :: Expr -> Expr -> Bool
propExprAddSorted a b = propExprIsSorted (add a b)

propExprEvalAdd :: Expr -> Expr -> Bool
propExprEvalAdd a b = eval id (a `add` b) == eval id a + eval id b

propExprMapId :: Expr -> Bool
propExprMapId a = mapExpr id id a == a
