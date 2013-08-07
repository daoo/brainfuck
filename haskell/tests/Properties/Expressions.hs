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

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = frequency
    [ (5, Const <$> arbitrary)
    , (1, Var <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

eq :: Eq a => Expr a -> Expr a -> Bool
eq (Const c1) (Const c2)         = c1 == c2
eq (Var n1 d1 xs) (Var n2 d2 ys) = n1 == n2 && d1 == d2 && eq xs ys
eq _ _                           = False

propExprIsSorted :: Expr Int -> Bool
propExprIsSorted = go True
  where
    go b = \case
      Const _                -> b
      Var _ _ (Const _)      -> b
      Var _ d1 (Var _ d2 xs) -> go (b && d1 <= d2) xs

propExprAddSorted :: Expr Int -> Expr Int -> Bool
propExprAddSorted a b = propExprIsSorted (add a b)

propExprEvalAdd :: Expr Int -> Expr Int -> Bool
propExprEvalAdd a b = eval id (a `add` b) == eval id a + eval id b

propExprMapId :: Expr Int -> Bool
propExprMapId a = mapExpr id id a `eq` a
