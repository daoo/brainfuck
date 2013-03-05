{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Assignment where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Data.Maybe
import Ext
import qualified Data.Graph as G
import qualified Data.Map as M

-- |Merge sequences of Set ILs using full program analysis
optimizeSets :: AST -> AST
optimizeSets = \case
  Nop -> Nop

  x@(Instruction (Set _ _) _) -> uncurry join $ modify $ splitSets x

  Instruction fun next -> Instruction fun (optimizeSets next)

  Flow ctrl inner next -> Flow ctrl (optimizeSets inner) (optimizeSets next)

  where
    modify (x, y) = (makeAST $ findOptimal x, optimizeSets y)

    splitSets = \case
      Instruction (Set d e) next -> mapFst ((d, e) :) $ splitSets next
      y                          -> ([], y)

    makeAST = foldr (Instruction . uncurry Set) Nop

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
findOptimal :: [SetOp] -> [SetOp]
findOptimal = topSort . go M.empty
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
