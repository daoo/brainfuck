{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr

propExprIsSorted :: Expr -> Bool
propExprIsSorted (Expr _ v) = go v
  where
    go = \case
      []                     -> True
      [_]                    -> True
      (_, d1) : (_, d2) : xs -> d1 <= d2 && go xs

propExprEvalConst :: Int -> Bool
propExprEvalConst c = eval undefined (constant c) == c

propExprEvalVar :: Int -> Bool
propExprEvalVar d = eval id (variable d) == d

propExprEvalAdd :: Expr -> Expr -> Bool
propExprEvalAdd a b = eval id (a `add` b) == eval id a + eval id b
