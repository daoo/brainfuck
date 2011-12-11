module Brainfuck.Compiler.Optimizer where

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizations

optimize :: [IL] -> [IL]
optimize []                                      = []
optimize (Loop l:xs)                             = Loop (optimize l) : optimize xs
optimize (s1@(Shift _) : il : s2@(Shift _) : xs) = reduceShifts s1 il s1 ++ optimize xs
optimize (il1 : il2 : xs)                        = mergeSame il1 il2 ++ optimize xs
optimize (x:xs)                                  = x : optimize xs
