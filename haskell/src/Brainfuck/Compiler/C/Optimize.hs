module Brainfuck.Compiler.C.Optimize where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = mapIL optimizeExpressions
         . inline
         . merge1 reduceLoops
         . merge3 joinThree
         . filterIL clean
         . merge2 applyShifts
         . merge2 sortIL
         . merge2 joinTwo
         . whileModified (merge2 shiftShifts)
         . whileModified (merge2 mergeSame)

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . whileModified optimize

