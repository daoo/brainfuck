module Brainfuck.Compiler.C.Optimize where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = filterIL clean
         . merge2 mergeSame
         . merge2 applyShifts
         . merge2 sortPokes
         . whileModified (merge2 shiftShifts)

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . whileModified optimize

