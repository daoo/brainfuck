{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Assignment
  ( optimizeAssign
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import qualified Data.Graph as G
import qualified Data.IntMap as M

-- |Finds sequenses of assignments and calculates the optimal representation
-- In this case optimal with respect to the number of assignment operations.
-- Thus we do not care about the size of the expressions.
optimizeAssign :: Tarpit -> Tarpit
optimizeAssign = go M.empty
  where
    go m = \case
      Nop -> makeOptimal m Nop

      Instruction (Assign d e) next -> go (M.insert d (rebuild m e) m) next

      Instruction fun next -> makeOptimal m $ Instruction fun (go M.empty next)
      Flow ctrl inner next -> makeOptimal m $ Flow ctrl (go M.empty inner) (go M.empty next)

-- |Inline only on each variable, instead of on the whole expression every time
rebuild :: M.IntMap Expr -> Expr -> Expr
rebuild m = go
  where
    go (Const c)    = Const c
    go (Var n d xs) = case M.lookup d m of
      Just e  -> add (mul n e) (go xs)
      Nothing -> Var n d (go xs)

type AssignOp = (Int, Expr)

-- |Calculate the optimal representation of some Assign ILs
-- TODO: Handle cyclical dependencies
makeOptimal :: M.IntMap Expr -> Tarpit -> Tarpit
makeOptimal ops next = mergeOps next $ topSort $ M.assocs ops

mergeOps :: Tarpit -> [AssignOp] -> Tarpit
mergeOps = foldr (Instruction . uncurry Assign)

-- Initial Code:
-- Assign 2 (Var 1)
-- Assign 1 (Var 0)
-- Assign 0 (Var 2)
--
-- 0: Var 1
-- 1: Var 0
-- 2: Var 1
--
-- After Optimal Assign:
-- Assign 2 (Var 1)
-- Assign 1 (Var 0)
-- Assign 0 (Var 1)
--
-- 0: Var 0
-- 1: Var 0
-- 2: Var 1
--
-- After Topologic Sort:
-- Assign 2 (Var 1)
-- Assign 0 (Var 1)
-- Assign 1 (Var 2)
--
-- 0; Var 1
-- 1: Var 1
-- 2: Var 1

topSort :: [AssignOp] -> [AssignOp]
topSort xs = let (graph, vertex, _) = G.graphFromEdges $ map f xs
              in map (g . vertex) $ G.topSort graph
  where
    f :: AssignOp -> (Expr, Int, [Int])
    f (d, e) = (e, d, edges e)

    g :: (Expr, Int, [Int]) -> AssignOp
    g (x, k, _) = (k, x)

    edges :: Expr -> [Int]
    edges = foldVarsR (\_ d -> (:) d) []
