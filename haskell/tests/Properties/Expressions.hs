{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr
import Brainfuck.Optimization.Inlining
import Control.Monad
import Data.Maybe
import Test.QuickCheck hiding (output)

constOnly :: Gen Expr
constOnly = frequency
  [ (3, liftM Const arbitrary)
  , (1, liftM2 Add constOnly constOnly)
  , (1, liftM2 Mul constOnly constOnly)
  ]

propExprOptimizeConst :: Property
propExprOptimizeConst = forAll constOnly (f . optimizeExpr)
  where
    f = \case
      Const _ -> True
      _       -> False

propExprOptimizeTwice :: Expr -> Bool
propExprOptimizeTwice e = let e' = optimizeExpr e in e' == optimizeExpr e'

propExprEval :: Expr -> NonEmptyList Int -> Bool
propExprEval e (NonEmpty xs) = eval f e == eval f (optimizeExpr e)
  where
    f = (!!) xs . (`mod` length xs)

propExprOptimizeSmaller :: Expr -> Bool
propExprOptimizeSmaller expr = exprComplexity expr >= exprComplexity (optimizeExpr expr)
