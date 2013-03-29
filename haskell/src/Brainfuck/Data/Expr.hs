{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import qualified Data.IntMap as M

type Expr = (Int, M.IntMap Int)

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 (c1, vars1) (c2, vars2) =
  n = M.lookup d1 vars2
  vars1' = M.map (*n) vars1
  (c1 * n + c2, M.merge vars1' vars2)
  where
    go = undefined

eval :: (Int -> Int) -> Expr -> Int
eval f = uncurry (foldrWithKey' (\k d s -> n * f d + s))
