{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr
import Brainfuck.Optimization.Inlining
import Control.Monad
import Test.QuickCheck

constOnly :: Gen Expr
constOnly = frequency
  [ (2, liftM mkInt arbitrary)
  , (1, liftM2 UnaryOp arbitrary constOnly)
  , (1, liftM3 BinaryOp arbitrary constOnly constOnly)
  ]

propExprOptimizeConst :: Property
propExprOptimizeConst = forAll constOnly (f . optimizeExpr)
  where
    f = \case
      Value (Const _) -> True
      _               -> False

propExprOptimizeTwice :: Expr -> Bool
propExprOptimizeTwice e = let e' = optimizeExpr e in e' == optimizeExpr e'

propExprEval :: Expr -> NonEmptyList Int -> Bool
propExprEval e (NonEmpty xs) = eval f e == eval f (optimizeExpr e)
  where
    f = (!!) xs . (`mod` length xs)

propExprOptimizeSmaller :: Expr -> Bool
propExprOptimizeSmaller expr = exprComplexity expr >= exprComplexity (optimizeExpr expr)
