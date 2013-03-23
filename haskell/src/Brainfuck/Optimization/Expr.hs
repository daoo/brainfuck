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
  Analysis 0 vars   -> clean $ M.foldrWithKey' f (mkInt 0) vars
  Analysis num vars -> M.foldrWithKey' f (mkInt num) vars
  where
    f _ 0 = id
    f d 1 = add (mkVar d)
    f d n = add (mkInt n `mul` mkVar d)

    -- HACK: hack to remove the last (+ e 0) that happens when num is 0
    clean = \case
      OperateBinary Add a (Return (Const 0)) -> a
      OperateBinary Add a b                  -> OperateBinary Add a $ clean b
      e                                      -> e

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
      Return (Const c) -> plus (factor * c)
      Return (Var d)   -> variable d factor

      OperateBinary Add a b -> go factor a >> go factor b

      OperateBinary Mul (Return (Const a)) b -> go (factor * a) b
      OperateBinary Mul a (Return (Const b)) -> go (factor * b) a

      _ -> fail "Expr2.analyse: not implemented"
