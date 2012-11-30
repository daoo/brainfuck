{-# LANGUAGE LambdaCase #-}
module Properties.Optimization where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Interpreter
import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.General
import Brainfuck.Optimization.Inlining
import Brainfuck.Optimization.Pipeline
import Data.ListZipper
import Data.Sequence (empty)
import Data.Word

compareFull :: (Integral a) => Int -> State a -> State a -> Bool
compareFull i (State _ out1 m1) (State _ out2 m2) = cut i m1 == cut i m2 && out1 == out2

compareOutput :: AST -> AST -> Bool
compareOutput xs ys = output (run state xs) == output (run state ys)
  where
    state :: State Word8
    state = State [1..] empty newMemory

testCode :: AST -> AST -> Bool
testCode xs ys = compareFull s (run state xs) (run state ys)
  where
    s = let (xsMin, xsMax) = memorySize xs
            (ysMin, ysMax) = memorySize ys
         in 1 + abs xsMin + abs xsMax + abs ysMin + abs ysMax

    state :: State Word8
    state = State [1..] empty newMemory

propOptimize :: (AST -> AST) -> AST -> Bool
propOptimize f xs = testCode xs (f xs)

-- TODO: Better testing

propInline :: Int -> Expr -> AST -> Bool
propInline d e xs = inline d e xs `testCode` Instruction (Set d e) xs

propHeuristicInlining :: Int -> Expr -> AST -> Bool
propHeuristicInlining d e xs = case heuristicInlining d e xs of
  Just xs' -> (Instruction (Set d e) xs) `testCode` xs'
  Nothing  -> True

propOptimizeInlineZeros, propOptimizeCopies, propOptimizeCleanUp,
  propOptimizeExpressions, propOptimizeMovePutGet, propOptimizeSets,
  propOptimizeMoveShifts :: AST -> Bool

propOptimizeCleanUp     = propOptimize cleanUp
propOptimizeCopies      = propOptimize reduceCopyLoops
propOptimizeExpressions = propOptimize optimizeExpressions
propOptimizeInlineZeros = propOptimize inlineZeros
propOptimizeMovePutGet  = propOptimize movePutGet
propOptimizeMoveShifts  = propOptimize moveShifts
propOptimizeSets        = propOptimize optimizeSets

propOptimizeAll :: AST -> Bool
propOptimizeAll xs = compareOutput xs (optimizeAll xs)
