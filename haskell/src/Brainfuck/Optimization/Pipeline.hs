module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Rewriting
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram
import Brainfuck.Utility

fullOptimization :: Tarpit -> Tarpit
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
                   , whileToIf
                   ])
                 . inlineZeros

simpleOptimizations :: Tarpit -> Tarpit
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
