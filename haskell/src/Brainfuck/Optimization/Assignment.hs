{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Assignment (optimizeAssign) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Utility
import Control.Arrow
import Data.List
import Data.Monoid
import qualified Data.Graph as G
import qualified Data.IntMap as M

-- |Merge sequences of Assign ILs using full program analysis
optimizeAssign :: Tarpit -> Tarpit
optimizeAssign = \case
  Nop -> Nop

  x@(Instruction (Assign _ _) _) -> uncurry mappend $ modify $ splitAssign x

  Instruction fun next -> Instruction fun (optimizeAssign next)
  Flow ctrl inner next -> Flow ctrl (optimizeAssign inner) (optimizeAssign next)

  where
    modify = (makeTarpit . findOptimal) *** optimizeAssign

splitAssign :: Tarpit -> ([AssignOp], Tarpit)
splitAssign = \case
  Instruction (Assign d e) next -> mapFst ((d, e) :) $ splitAssign next
  y                             -> ([], y)

makeTarpit :: [AssignOp] -> Tarpit
makeTarpit = foldr (Instruction . uncurry Assign) Nop

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

type AssignOp = (Int, Expr)

-- |Calculate the optimal representation of some Assign ILs
-- TODO: Handle cyclical dependencies
findOptimal :: [AssignOp] -> [AssignOp]
findOptimal = topSort . inlineOps

inlineOps :: [AssignOp] -> [AssignOp]
inlineOps = M.assocs . foldl' (\m (x, e) -> M.insert x (rebuild m e) m) M.empty

-- |Inline only on each variable, instead of on the whole expression every time
rebuild :: M.IntMap Expr -> Expr -> Expr
rebuild m = go
  where
    go (Const c)    = Const c
    go (Var n d xs) = case M.lookup d m of
      Just e  -> add (mapExpr (mapFst (*n)) (*n) e) (go xs)
      Nothing -> Var n d (go xs)

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
