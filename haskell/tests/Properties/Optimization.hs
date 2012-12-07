{-# LANGUAGE LambdaCase #-}
module Properties.Optimization where

import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.General
import Brainfuck.Optimization.Pipeline
import Code

propOptimizeInlineZeros, propOptimizeCopies, propOptimizeCleanUp,
  propOptimizeExpressions, propOptimizeMovePutGet, propOptimizeSets,
  propOptimizeMoveShifts :: PrettyAST -> Bool

propOptimizeCleanUp     = checkTransform cleanUp . getAst
propOptimizeCopies      = checkTransform reduceCopyLoops . getAst
propOptimizeExpressions = checkTransform optimizeExpressions . getAst
propOptimizeInlineZeros = checkTransform inlineZeros . getAst
propOptimizeMovePutGet  = checkTransform movePutGet . getAst
propOptimizeMoveShifts  = checkTransform moveShifts . getAst
propOptimizeSets        = checkTransform optimizeSets . getAst

propOptimizeAll :: PrettyAST -> Bool
propOptimizeAll (PrettyAST ast) = testCode compareOutput ast (optimizeAll ast)
