module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram

loopUnrolling :: Tarpit -> Tarpit
loopUnrolling = unrollEntierly . fullOptimization

fullOptimization :: Tarpit -> Tarpit
fullOptimization = removeFromEnd
                 . optimizeAssign
                 . inlineConstants
                 . inlineZeros
                 . putReduction
                 . flowReduction
                 . whileReduction
                 . copyLoopReduction
                 . inlineShifts

simpleOptimizations :: Tarpit -> Tarpit
simpleOptimizations = inlineZeros
                    . flowReduction
                    . inlineShifts
                    . inlineZeros
