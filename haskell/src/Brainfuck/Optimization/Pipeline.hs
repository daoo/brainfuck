module Brainfuck.Optimization.Pipeline where

import Brainfuck.Data.AST
import Brainfuck.Optimization.AST
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.Rewriting
import Brainfuck.Utility

fullOptimization :: AST -> AST
fullOptimization = once expressions . optimizeAssign . tryMaybe (rewrite astRules)
