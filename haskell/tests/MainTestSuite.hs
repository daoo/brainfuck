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
      , testProperty "Sorted after add" propExprAddSorted
      , testProperty "Eval add" propExprEvalAdd
      , testProperty "Map id" propExprMapId
      ]
    ]
  ]
