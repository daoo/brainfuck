{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Assignment where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Optimization.Rewriting
import Data.Maybe
import Ext
import qualified Data.Graph as G
import qualified Data.Map as M

-- |Merge sequences of Set ILs
optimizeSets :: AST -> Rule AST
optimizeSets x@(Instruction (Set _ _) _) = return $ uncurry join $ mapFst (mergeSets . optimalSets) $ splitSets x
  where
    splitSets = \case
      Instruction (Set d e) next -> mapFst ((d, e) :) $ splitSets next
      y                          -> ([], y)

    mergeSets = foldr (\(d, e) x' -> Instruction (Set d e) x') Nop

optimizeSets ast = fail (show ast)

-- Initial Code:
-- Set 2 (Get 1)
-- Set 1 (Get 0)
-- Set 0 (Get 2)
--
-- 0: Get 1
-- 1: Get 0
-- 2: Get 1
--
-- After Optimal Sets:
-- Set 2 (Get 1)
-- Set 1 (Get 0)
-- Set 0 (Get 1)
--
-- 0: Get 0
-- 1: Get 0
-- 2: Get 1
--
-- After Topologic Sort:
-- Set 2 (Get 1)
-- Set 0 (Get 1)
-- Set 1 (Get 2)
--
-- 0; Get 1
-- 1: Get 1
-- 2: Get 1

type SetOp = (Int, Expr)

-- |Calculate the optimal representation of some Set ILs
-- TODO: Handle cyclical dependencies
optimalSets :: [SetOp] -> [SetOp]
optimalSets = topSort . go M.empty
  where
    go :: M.Map Int Expr -> [SetOp] -> [SetOp]
    go m []          = M.assocs m
    go m ((x, e):xs) = go (M.alter (const $ Just $ f m e) x m) xs

    f :: M.Map Int Expr -> Expr -> Expr
    f m = modifyValues (\case
      e@(Get i) -> fromMaybe (Value e) $ M.lookup i m
      e         -> Value e)

topSort :: [SetOp] -> [SetOp]
topSort xs = map ((\(x, k, _) -> (k, x)) . f) $ G.topSort $ graph
  where
    (graph, f, _) = G.graphFromEdges $ map (\(d, e) -> (e, d, get e)) xs

    get = unfold (flip const) (const (++)) (\case
      Get d -> [d]
      _     -> [])
