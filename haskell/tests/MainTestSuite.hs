module Main where

import Properties.Compiler
import Properties.Expressions
import Properties.Misc
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
    , testGroup "Programs"
      [ testProperty "Reverse" propReverse
      , testProperty "ASCII Values" propASCIIValues
      ]
    , testGroup "Expressions"
      [ testProperty "Is sorted" propExprIsSorted
      , testProperty "Eval constant" propExprEvalConst
      , testProperty "Eval variable" propExprEvalVar
      , testProperty "Eval add" propExprEvalAdd
      ]
    ]
  ]
