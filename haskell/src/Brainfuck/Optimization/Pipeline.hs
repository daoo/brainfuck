module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram
import Brainfuck.Utility

loopUnrolling :: Tarpit -> Tarpit
loopUnrolling = times fullOptimization 1000 . unrollLoops 100 . fullOptimization

fullOptimization :: Tarpit -> Tarpit
fullOptimization = inlineConstants
                 . movePut
                 . inlineZeros
                 . optimizeAssign
                 . removeFromEnd
                 . flowReduction
                 . whileToIf
                 . copyLoopReduction
                 . shiftReduction
                 . inlineZeros

simpleOptimizations :: Tarpit -> Tarpit
simpleOptimizations = inlineZeros
                    . flowReduction
                    . shiftReduction
                    . inlineZeros
