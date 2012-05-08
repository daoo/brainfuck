module Brainfuck.Compiler.C.Optimize where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizing
import Brainfuck.Ext

optimize :: [IL] -> [IL]
optimize = mapIL optimizeExpressions
         . filterIL clean
         . inline

optimizeForC :: [IL] -> [IL]
optimizeForC = removeFromEnd . times optimize 100

