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
