module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.AST
import Brainfuck.Optimization.AST
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Rewriting
import Brainfuck.Optimization.WholeProgram
import Brainfuck.Utility

fullOptimization :: AST -> AST
fullOptimization = removeFromEnd
                 . once expressions
                 . inlineZeros
                 . optimizeAssign
                 . tryMaybe (rewrite
                   [ reflectiveAssign
                   , shiftZero
                   , flowInnerNop
                   , flowNever
                   , flowOnce
                   , flowConst
                   , movePut
                   , moveShifts
                   , reduceCopyLoops
                   -- , whileToIf
                   ])
                 . inlineZeros

simpleOptimizations :: AST -> AST
simpleOptimizations = once expressions
                    . inlineZeros
                    . tryMaybe (rewrite
                      [ reflectiveAssign
                      , shiftZero
                      , flowInnerNop
                      , flowNever
                      , flowOnce
                      , flowConst
                      , movePut
                      , moveShifts
                      ])
                    . inlineZeros
