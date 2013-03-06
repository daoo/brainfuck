module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.AST
import Brainfuck.Optimization.AST
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Rewriting
import Ext

fullOptimization :: AST -> AST
fullOptimization = tryMaybe (rewrite [expressions]) . optimizeSets . tryMaybe (rewrite astRules)
