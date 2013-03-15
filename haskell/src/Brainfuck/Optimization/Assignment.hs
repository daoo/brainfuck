{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Assignment where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Data.Maybe
import Ext
import qualified Data.Graph as G
import qualified Data.Map as M

-- |Merge sequences of Assign ILs using full program analysis
optimizeAssign :: AST -> AST
optimizeAssign = \case
  Nop -> Nop

  x@(Instruction (Assign _ _) _) -> uncurry join $ modify $ splitAssign x

  Instruction fun next -> Instruction fun (optimizeAssign next)

  Flow ctrl inner next -> Flow ctrl (optimizeAssign inner) (optimizeAssign next)

  where
    modify (x, y) = (makeAST $ findOptimal x, optimizeAssign y)

    splitAssign = \case
      Instruction (Assign d e) next -> mapFst ((d, e) :) $ splitAssign next
      y                             -> ([], y)

    makeAST = foldr (Instruction . uncurry Assign) Nop

-- Initial Code:
-- Assign 2 (Get 1)
-- Assign 1 (Get 0)
-- Assign 0 (Get 2)
--
-- 0: Get 1
-- 1: Get 0
-- 2: Get 1
--
-- After Optimal Assign:
-- Assign 2 (Get 1)
-- Assign 1 (Get 0)
-- Assign 0 (Get 1)
--
-- 0: Get 0
-- 1: Get 0
-- 2: Get 1
--
-- After Topologic Sort:
-- Assign 2 (Get 1)
-- Assign 0 (Get 1)
-- Assign 1 (Get 2)
--
-- 0; Get 1
-- 1: Get 1
-- 2: Get 1

type AssignOp = (Int, Expr)

-- |Calculate the optimal representation of some Assign ILs
-- TODO: Handle cyclical dependencies
findOptimal :: [AssignOp] -> [AssignOp]
findOptimal = topSort . go M.empty
  where
    go :: M.Map Int Expr -> [AssignOp] -> [AssignOp]
    go m []          = M.assocs m
    go m ((x, e):xs) = go (M.alter (const $ Just $ f m e) x m) xs

    f :: M.Map Int Expr -> Expr -> Expr
    f m = modifyValues (\case
      e@(Get i) -> fromMaybe (Return e) $ M.lookup i m
      e         -> Return e)

topSort :: [AssignOp] -> [AssignOp]
topSort xs = map ((\(x, k, _) -> (k, x)) . f) $ G.topSort $ graph
  where
    (graph, f, _) = G.graphFromEdges $ map (\(d, e) -> (e, d, get e)) xs

    get = unfold (flip const) (const (++)) (\case
      Get d -> [d]
      _     -> [])
