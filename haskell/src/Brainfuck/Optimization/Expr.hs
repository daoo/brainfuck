{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Expr (simplify) where

import Brainfuck.Data.Expr
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as M

simplify :: Expr -> Expr
simplify = toExpr . analyse

data Analysis = Analysis
  { number :: Int
  , variables :: M.IntMap Int
  } deriving Show

toExpr :: Analysis -> Expr
toExpr = \case
  -- This case exists to remove the last Const 0 from the expression
  Analysis 0 vars -> case M.toList vars of

    []         -> Const 0
    ((d,n):xs) -> foldr (uncurry f) (n `Mul` Var d) xs

  Analysis num vars -> M.foldrWithKey' f (Const num) vars

  where
    f _ 0 = id
    f d 1 = Add (Var d)
    f d n = Add (n `Mul` Var d)

plus :: Int -> State Analysis ()
plus c = modify (\a -> a { number = number a + c })

variable :: Int -> Int -> State Analysis ()
variable k v = modify (\a -> a { variables = M.insertWith (+) k v (variables a) })

-- |Break an Expr into a constant and multiples of variables
-- Works under the assumption that we can't multiply variables with each other,
-- only with constants.
analyse :: Expr -> Analysis
analyse e = execState (go 1 e) (Analysis 0 M.empty)
  where
    go :: Int -> Expr -> State Analysis ()
    go factor = \case
      Const c -> plus (factor * c)
      Var d   -> variable d factor

      Add a b -> go factor a >> go factor b
      Mul a b -> go (factor * a) b
