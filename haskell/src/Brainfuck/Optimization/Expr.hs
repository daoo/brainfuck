{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Expr where

import Brainfuck.Data.Expr
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

simplify :: Expr -> Expr
simplify = toExpr . analyse

data Analysis = Analysis
  { number :: Int
  , variables :: M.Map Int Int
  } deriving Show

toExpr :: Analysis -> Expr
toExpr = \case
  Analysis 0 vars   -> clean $ M.foldrWithKey' f (Const 0) vars
  Analysis num vars -> M.foldrWithKey' f (Const num) vars
  where
    f _ 0 = id
    f d 1 = Add (Var d)
    f d n = Add (n `Mul` Var d)

    -- HACK: hack to remove the last (+ e 0) that happens when num is 0
    clean = \case
      Add a (Const 0) -> a
      Add a b         -> Add a $ clean b
      e               -> e

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
