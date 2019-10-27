{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Assignment
  ( optimizeAssign
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import qualified Data.Graph as G
import qualified Data.IntMap as M

-- |Finds sequenses of assignments and calculates the optimal representation.
-- In this case optimal with respect to the number of assignment operations.
-- That means we do not care about the size of the expressions.
optimizeAssign :: Tarpit -> Tarpit
optimizeAssign = go M.empty
  where
    go m = \case
      Nop -> makeOptimal m Nop

      Instruction (Assign d e) next -> go (M.insert d (insertExprs e m) m) next

      Instruction fun next -> makeOptimal m $ Instruction fun (go M.empty next)
      Flow ctrl inner next -> makeOptimal m $ Flow ctrl (go M.empty inner) (go M.empty next)

type AssignOp = (Int, Expr)

-- |Calculate the optimal representation of some Assign ILs.
makeOptimal :: M.IntMap Expr -> Tarpit -> Tarpit
makeOptimal ops next = mergeOps next $ topSort $ M.assocs ops
-- TODO: Handle cyclical dependencies

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
-- 0: Var 1
-- 1: Var 1
-- 2: Var 1

topSort :: [AssignOp] -> [AssignOp]
topSort xs = let (graph, vertex, _) = G.graphFromEdges $ map mkvert xs
              in map (retrieve . vertex) $ G.topSort graph
  where
    mkvert (d, e) = (e, d, mkedges e)
    mkedges = foldExpr (const (:)) (const [])

    retrieve (x, k, _) = (k, x)
