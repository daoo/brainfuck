{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr
import Brainfuck.Optimization.Expr
import Brainfuck.Optimization.Inlining
import Brainfuck.Optimization.Rewriting
import Control.Applicative ((<$>),(<*>))
import Ext
import Test.QuickCheck

constOnly :: Gen Expr
constOnly = sized $ \n -> expr n n
  where
    expr 0 _ = leaf
    expr n s = oneof [leaf, branch n s]

    branch n s = frequency
      [ (1, OperateUnary <$> arbitrary <*> (expr (n - 1) s))
      , (4, OperateBinary <$> arbitrary <*> (expr (n - 1) s) <*> (expr (n - 1) s))
      ]

    leaf = mkInt <$> arbitrary

propExprOptimizeConst :: Property
propExprOptimizeConst = forAll constOnly (f . tryMaybe (rewrite exprRules))
  where
    f = \case
      Value (Const _) -> True
      _               -> False

propExprOptimizeTwice :: Expr -> Bool
propExprOptimizeTwice e = let e' = (tryMaybe (rewrite exprRules)) e in e' == (tryMaybe (rewrite exprRules)) e'

propExprEval :: Expr -> NonEmptyList Int -> Bool
propExprEval e (NonEmpty xs) = eval f e == eval f (tryMaybe (rewrite exprRules) e)
  where
    f = (!!) xs . (`mod` length xs)

propExprOptimizeSmaller :: Expr -> Bool
propExprOptimizeSmaller expr = exprComplexity expr >= exprComplexity (tryMaybe (rewrite exprRules) expr)
