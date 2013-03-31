module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Rewriting
import Brainfuck.Optimization.Tarpit
import Brainfuck.Optimization.WholeProgram
import Brainfuck.Utility

fullOptimization :: Tarpit -> Tarpit
fullOptimization = inlineZeros
                 . optimizeAssign
                 . removeFromEnd
                 . flowReduction
                 . tryMaybe (rewrite
                   [ movePut
                   , reduceCopyLoops
                   , whileToIf
                   ])
                 . shiftReduction
                 . inlineZeros

simpleOptimizations :: Tarpit -> Tarpit
simpleOptimizations = inlineZeros
                    . flowReduction
                    . tryMaybe (rewrite [movePut])
                    . shiftReduction
                    . inlineZeros
