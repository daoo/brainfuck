{-# LANGUAGE LambdaCase #-}
module Properties.Expressions
  ( propExprIsSorted
  , propExprAddSorted
  , propExprEvalAdd
  , propExprMapId
  , Arbitrary
  ) where

import Brainfuck.Data.Expr
import Control.Applicative hiding (Const)
import Test.QuickCheck

propExprIsSorted :: Expr Int Int -> Bool
propExprIsSorted = go True
  where
    go b = \case
      Const _                -> b
      Var _ _ (Const _)      -> b
      Var _ d1 (Var _ d2 xs) -> go (b && d1 <= d2) xs

propExprAddSorted :: Expr Int Int -> Expr Int Int -> Bool
propExprAddSorted a b = propExprIsSorted (add a b)

propExprEvalAdd :: Expr Int Int -> Expr Int Int -> Bool
propExprEvalAdd a b = eval id (a `add` b) == eval id a + eval id b

propExprMapId :: Expr Int Int -> Bool
propExprMapId a = mapExpr id id a `eq` a
