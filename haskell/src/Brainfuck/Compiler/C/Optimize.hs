module Brainfuck.Compiler.C.Optimize where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = merge1 reduceLoops
         . filterIL clean
         . merge2 applyShifts
         . merge2 sortMutators
         . merge2 miscJoins
         . whileModified (merge2 shiftShifts)
         . whileModified (merge2 mergeSame)

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . whileModified optimize

