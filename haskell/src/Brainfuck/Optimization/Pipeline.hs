module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram

fullOptimization :: Tarpit -> Tarpit
fullOptimization = inlineConstants
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
