module Properties.Misc where

import Control.Applicative
import Data.ListZipper
import Test.QuickCheck

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary = ListZipper <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (ListZipper xs y zs) = map (\(xs', zs') -> ListZipper xs' y zs')
                              $ zip (shrink xs) (shrink zs)

propZipperMoveSize :: ListZipper Int -> Property
propZipperMoveSize a@(ListZipper xs _ zs) =
  not (null xs) && not (null zs) ==>
    forAll (choose (-m, m)) $ \i -> size a == size (move i a)
  where
    m = min (length xs) (length zs)
