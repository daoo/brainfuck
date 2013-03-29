{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Expr (simplify) where

import Brainfuck.Data.Expr
import qualified Data.IntMap.Strict as M

simplify :: Expr -> Expr
simplify = toExpr . analyse

type Analysis = (Int, M.IntMap Int)

toExpr :: Analysis -> Expr
toExpr = \case
  -- This case exists to remove the last Const 0 from the expression
  (0, vars) -> case M.toList vars of

    []         -> Const 0
    ((d,n):xs) -> foldr (uncurry f) (n `Mul` Var d) xs

  (num, vars) -> M.foldrWithKey' f (Const num) vars

  where
    f _ 0 = id
    f d 1 = Add (Var d)
    f d n = Add (n `Mul` Var d)

-- |Break an Expr into a constant and multiples of variables
-- Works under the assumption that we can't multiply variables with each other,
-- only with constants.
analyse :: Expr -> Analysis
analyse = go 1 0 M.empty
  where
    go :: Int -> Int -> M.IntMap Int -> Expr -> (Int, M.IntMap Int)
    go factor num vars = \case
      Const c -> (num + factor * c, vars)
      Var d   -> (num, M.insertWith (+) d factor vars)

      Add a b -> let (num', vars') = go factor num vars a
                  in go factor num' vars' b

      Mul a b -> go (factor * a) num vars b
