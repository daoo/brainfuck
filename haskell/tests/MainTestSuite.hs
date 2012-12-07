module Main where

import Properties.Compiler
import Properties.Expressions
import Properties.Misc
import Properties.Optimization
import Properties.Programs
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "QuickCheck"
    [ testGroup "Misc"
      [ testProperty "ListZipper move size" propZipperMoveSize
      , testProperty "Map Index const x" propMapIndexEq
      , testProperty "Map Index length invariant" propMapIndexLength
      , testProperty "Pipe add invariant" propPipeAdd
      , testProperty "While Modified towards zero" propWhileModified
      ]
    , testGroup "Compiler"
      [ testProperty "Compile Decompile invariant" propCompileDecompile
      , testProperty "Parse Show invariant" propParser
      ]
    , testGroup "Expressions"
      [ testProperty "Optimize Const Expression" propExprOptimizeConst
      , testProperty "Optimize Twice Invariant" propExprOptimizeTwice
      , testProperty "Optimize Decerase Size" propExprOptimizeSmaller
      , testProperty "Evaluation" propExprEval
      ]
    , testGroup "Optimization"
      [ testProperty "Clean Up" propOptimizeCleanUp
      , testProperty "Copy Loop Reduction" propOptimizeCopies
      , testProperty "Expressions" propOptimizeExpressions
      , testProperty "Inline Initial Values" propOptimizeInlineZeros
      , testProperty "Move Put and Get" propOptimizeMovePutGet
      , testProperty "Move Shifts" propOptimizeMoveShifts
      , testProperty "Minimal Assignments" propOptimizeSets
      , testProperty "All Optimizations" propOptimizeAll
      ]
    , testGroup "Programs"
      [ testProperty "Reverse" propReverse
      , testProperty "ASCII Values" propASCIIValues
      ]
    ]
  ]
