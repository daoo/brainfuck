module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit

import Properties.Compiler
import Properties.Expressions
import Properties.Misc
import Properties.Optimization

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
    ]
  ]
