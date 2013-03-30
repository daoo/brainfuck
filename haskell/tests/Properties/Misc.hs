{-# LANGUAGE LambdaCase #-}
module Properties.Misc where

import Brainfuck.Utility
import Data.ListZipper
import Test.QuickCheck

propZipperMoveSize :: ListZipper Int -> Property
propZipperMoveSize a@(ListZipper xs _ zs) =
  not (null xs) && not (null zs) ==>
    forAll (choose (-m, m)) $ \i -> size a == size (move i a)
  where
    m = min (length xs) (length zs)

propMapIndexEq :: Int -> NonEmptyList Int -> Property
propMapIndexEq x (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \i -> (mapIndex (const x) i xs !! i) == x

propMapIndexLength :: Int -> NonEmptyList Int -> Property
propMapIndexLength x (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \i -> length (mapIndex (const x) i xs) == length xs

propPipeAdd :: Property
propPipeAdd = forAll (choose (0, 1000)) $
  \i -> pipe (replicate i (+1)) 0 == i

propWhileModified :: Property
propWhileModified = forAll (choose (-1000, 1000)) ((== 0) . whileModified f)
  where
    f :: Int -> Int
    f j | j < 0     = j + 1
        | j == 0    = 0
        | otherwise = j - 1
