module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram
import Brainfuck.Utility

loopUnrolling :: Tarpit -> Tarpit
loopUnrolling = removeFromEnd
              . movePut
              . times ( inlineConstants
                      . inlineZeros
                      . shiftReduction
                      . unrollLoop
                      ) 100
              . fullOptimization

fullOptimization :: Tarpit -> Tarpit
fullOptimization = removeFromEnd
                 . optimizeAssign
                 . movePut
                 . flowReduction
                 . inlineConstants
                 . inlineZeros
                 . whileToIf
                 . copyLoopReduction
                 . shiftReduction

simpleOptimizations :: Tarpit -> Tarpit
simpleOptimizations = inlineZeros
                    . flowReduction
                    . shiftReduction
                    . inlineZeros
