module Brainfuck.Compiler.C.Optimize where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = merge1 reduceLoops
         . filterIL clean
         . merge2 mergeSame
         . merge2 applyShifts
         . merge2 sortMutators
         . whileModified (merge2 shiftShifts)

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . whileModified optimize

