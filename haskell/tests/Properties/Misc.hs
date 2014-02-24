module Properties.Misc
  ( propZipperMoveSize
  , propZipperMoveInv
  , propZipperLeftRight
  , propZipperRightLeft
  ) where

import Control.Applicative
import Data.ListZipper
import Test.QuickCheck

genNonEmpty :: Arbitrary a => Gen (ListZipper a)
genNonEmpty = ListZipper <$> listOf1 arbitrary <*> arbitrary <*> listOf1 arbitrary

genMinLen :: ListZipper a -> Gen Int
genMinLen (ListZipper xs _ zs) = choose (-m, m)
  where
    m = min (length xs) (length zs)

propZipperMoveSize :: Property
propZipperMoveSize = forAll (genNonEmpty :: Gen (ListZipper ())) $
  \a -> forAll (genMinLen a) $
    \i -> size a == size (move i a)

propZipperMoveInv :: Property
propZipperMoveInv = forAll (genNonEmpty :: Gen (ListZipper ())) $
  \a -> forAll (genMinLen a) $
    \i -> a == move (-i) (move i a)

propZipperLeftRight :: Property
propZipperLeftRight = forAll (genNonEmpty :: Gen (ListZipper ())) $
  \a -> a == moveRight (moveLeft a)

propZipperRightLeft :: Property
propZipperRightLeft = forAll (genNonEmpty :: Gen (ListZipper ())) $
  \a -> a == moveLeft (moveRight a)
