{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr
import Brainfuck.Optimization.Expression
import Brainfuck.Optimization.Inlining
import Brainfuck.Optimization.Rule
import Control.Applicative ((<$>),(<*>))
import Test.QuickCheck

constOnly :: Gen Expr
constOnly = sized $ \n -> expr n n
  where
    expr 0 _ = leaf
    expr n s = oneof [leaf, branch n s]

    branch n s = frequency
      [ (1, UnaryOp <$> arbitrary <*> (expr (n - 1) s))
      , (4, BinaryOp <$> arbitrary <*> (expr (n - 1) s) <*> (expr (n - 1) s))
      ]

    leaf = mkInt <$> arbitrary

propExprOptimizeConst :: Property
propExprOptimizeConst = forAll constOnly (f . (perhaps (loop exprRules)))
  where
    f = \case
      Value (Const _) -> True
      _               -> False

propExprOptimizeTwice :: Expr -> Bool
propExprOptimizeTwice e = let e' = (perhaps (loop exprRules)) e in e' == (perhaps (loop exprRules)) e'

propExprEval :: Expr -> NonEmptyList Int -> Bool
propExprEval e (NonEmpty xs) = eval f e == eval f (perhaps (loop exprRules) e)
  where
    f = (!!) xs . (`mod` length xs)

propExprOptimizeSmaller :: Expr -> Bool
propExprOptimizeSmaller expr = exprComplexity expr >= exprComplexity (perhaps (loop exprRules) expr)
