module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram

loopUnrolling :: Tarpit -> Tarpit
loopUnrolling = unrollEntierly

fullOptimization :: Tarpit -> Tarpit
fullOptimization = removeFromEnd
                 . optimizeAssign
                 . inlineConstants
                 . inlineZeros
                 . reduce (putReduction `pipe` flowReduction `pipe` whileReduction `pipe` copyLoopReduction)
                 . inlineShifts

simpleOptimizations :: Tarpit -> Tarpit
simpleOptimizations = inlineZeros
                    . reduce flowReduction
                    . inlineShifts
                    . inlineZeros
