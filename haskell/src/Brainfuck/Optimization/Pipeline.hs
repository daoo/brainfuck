module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.AST
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.General
import Ext

optimizeAll :: AST -> AST
optimizeAll = removeFromEnd . whileModified pipeline
  where
    pipeline = optimizeSets
             . whileToIf
             . reduceCopyLoops
             . moveShifts
             . optimizeExpressions
             . inlineZeros
             . movePutGet
             . cleanUp
