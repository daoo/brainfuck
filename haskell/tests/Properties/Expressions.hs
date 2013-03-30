{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr
import Test.QuickCheck
import qualified Data.IntMap as M

instance Arbitrary a => Arbitrary (M.IntMap a) where
  arbitrary = M.fromList `fmap` arbitrary

propExprEvalConst :: Int -> Bool
propExprEvalConst c = eval undefined (constant c) == c

propExprEvalVar :: Int -> Bool
propExprEvalVar d = eval id (variable d) == d

propExprEmptyVariables :: Bool
propExprEmptyVariables = eval undefined (variables []) == 0

propExprVarAnalysis :: Int -> Bool
propExprVarAnalysis d = varAnalysis (variable d) == Just d

propExprEvalAdd :: Expr -> Expr -> Bool
propExprEvalAdd a b = eval id (a `add` b) == eval id a + eval id b

propExprEvalInl :: Int -> Int -> Bool
propExprEvalInl d c = let a = constant c
                          b = variable d
                       in eval id (inlineExpr d a b) == eval (const (eval id a)) b
