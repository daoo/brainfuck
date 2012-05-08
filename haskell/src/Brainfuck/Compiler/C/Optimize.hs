module Brainfuck.Compiler.C.Optimize where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = mapIL optimizeExpressions
         . inline
         . merge1 reduceLoops
         . filterIL clean
         . merge2 applyShifts
         . whileModified (merge2 shiftShifts)
         . whileModified (merge2 mergeSame)

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . times optimize 10

