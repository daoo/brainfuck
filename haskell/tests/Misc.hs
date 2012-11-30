{-# LANGUAGE LambdaCase #-}
module Properties where

import Data.ListZipper
import Ext
import Test.QuickCheck hiding (output)

propZipperMoveSize :: Int -> ListZipper a -> Bool
propZipperMoveSize i a = size a == size (move i a)

propMapIndexEq :: Int -> NonEmptyList Int -> Property
propMapIndexEq x (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \i -> (mapIndex (const x) i xs !! i) == x

propMapIndexLength :: Int -> NonEmptyList Int -> Property
propMapIndexLength x (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \i -> length (mapIndex (const x) i xs) == length xs

propPipeAdd :: Property
propPipeAdd = forAll (choose (0, 20000)) $
  \i -> pipe (replicate i (+1)) 0 == i

propWhileModified :: Property
propWhileModified = forAll (choose (-10000000, 10000000)) ((== 0) . whileModified f)
  where
    f :: Int -> Int
    f j | j < 0     = j + 1
        | j == 0    = 0
        | otherwise = j - 1
