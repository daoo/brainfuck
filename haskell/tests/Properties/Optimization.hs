{-# LANGUAGE LambdaCase #-}
module Properties.Optimization where

import Brainfuck.CodeGen.Indented
import Brainfuck.Data.AST
import Brainfuck.Interpreter
import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.General
import Brainfuck.Optimization.Pipeline
import Data.ListZipper
import Data.Sequence (empty)
import Data.Word
import Test.QuickCheck hiding (output)

newtype PrettyAST = PrettyAST { getAst :: AST }

instance Arbitrary PrettyAST where
  arbitrary = fmap PrettyAST arbitrary

instance Show PrettyAST where
  show (PrettyAST ast) = showIndented ast

compareFull :: (Integral a) => Int -> State a -> State a -> Bool
compareFull i (State _ out1 m1) (State _ out2 m2) =
  cut i m1 == cut i m2 && out1 == out2

compareOutput :: (Integral a) => State a -> State a -> Bool
compareOutput s1 s2 = output s1 == output s2

testCode :: (State Word8 -> State Word8 -> Bool) -> AST -> AST -> Bool
testCode f ast ast' = f (run state ast) (run state ast')
  where
    state :: State Word8
    state = State [1..] empty newMemory

propTransform :: (AST -> AST) -> AST -> Bool
propTransform f ast = testCode (compareFull s) ast ast'
  where
    ast' = f ast

    s = let (xsMin, xsMax) = memorySize ast
            (ysMin, ysMax) = memorySize ast'
         in 1 + abs xsMin + abs xsMax + abs ysMin + abs ysMax

propOptimizeInlineZeros, propOptimizeCopies, propOptimizeCleanUp,
  propOptimizeExpressions, propOptimizeMovePutGet, propOptimizeSets,
  propOptimizeMoveShifts :: PrettyAST -> Bool

propOptimizeCleanUp     = propTransform cleanUp . getAst
propOptimizeCopies      = propTransform reduceCopyLoops . getAst
propOptimizeExpressions = propTransform optimizeExpressions . getAst
propOptimizeInlineZeros = propTransform inlineZeros . getAst
propOptimizeMovePutGet  = propTransform movePutGet . getAst
propOptimizeMoveShifts  = propTransform moveShifts . getAst
propOptimizeSets        = propTransform optimizeSets . getAst

propOptimizeAll :: PrettyAST -> Bool
propOptimizeAll (PrettyAST ast) = testCode compareOutput ast (optimizeAll ast)
