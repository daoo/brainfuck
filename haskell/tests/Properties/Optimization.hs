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
import Data.Sequence
import Data.Word
import Test.QuickCheck hiding (output)

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
propOptimizeAll (PrettyAST ast) = wasCorrect $ testCode compareOutput ast (optimizeAll ast)
