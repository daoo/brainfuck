module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram
import Brainfuck.Utility

loopUnrolling :: Tarpit -> Tarpit
loopUnrolling = fullOptimization
              . times ( flowReduction
                      . inlineConstants
                      . inlineZeros
                      . shiftReduction
                      ) 400
              . unrollLoops 100
              . fullOptimization

fullOptimization :: Tarpit -> Tarpit
fullOptimization = removeFromEnd
                 . movePut
                 . flowReduction
                 . inlineConstants
                 . inlineZeros
                 . optimizeAssign
                 . whileToIf
                 . copyLoopReduction
                 . shiftReduction

simpleOptimizations :: Tarpit -> Tarpit
simpleOptimizations = inlineZeros
                    . flowReduction
                    . shiftReduction
                    . inlineZeros
