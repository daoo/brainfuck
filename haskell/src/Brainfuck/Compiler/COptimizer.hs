module Brainfuck.Compiler.COptimizer where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = merge2 mergeSame
         . merge2 sortPokes
         . filterIL clean
         . whileModified (merge2 shiftShifts)

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . whileModified optimize

