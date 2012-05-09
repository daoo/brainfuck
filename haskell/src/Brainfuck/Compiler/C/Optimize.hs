module Brainfuck.Compiler.C.Optimize where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = reduceLoops
         . mapIL optimizeExpressions
         . filterIL clean
         . applyIL

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . times optimize 100

