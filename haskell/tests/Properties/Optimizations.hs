module Properties.Optimizations where

import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Assignment
import Test.QuickCheck
import Utility

propOptAssign :: Property
propOptAssign = forAll assignments $ \code ->
  tests [testMemory $ (\ (a, b) -> abs a + b) (memorySize code)] code (optimizeAssign code)
