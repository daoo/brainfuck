module Main where

import Properties.Compiler
import Properties.Expressions
import Properties.Misc
import Properties.Programs
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck"
  [ testGroup "Misc"
    [ testProperty "ListZipper move size" propZipperMoveSize
    , testProperty "ListZipper move inversion" propZipperMoveInv
    , testProperty "ListZipper left right" propZipperLeftRight
    , testProperty "ListZipper right left" propZipperRightLeft
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
    , testProperty "Eval mul" propExprEvalMul
    ]
  ]
