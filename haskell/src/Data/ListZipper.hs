module Data.ListZipper where

import Test.QuickCheck.Arbitrary

data ListZipper a = ListZipper
  { left :: [a]
  , focus :: a
  , right :: [a] }
  deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (ListZipper a) where
  arbitrary = do
    xs <- arbitrary
    y  <- arbitrary
    zs <- arbitrary
    return $ ListZipper xs y zs

move :: Int -> ListZipper a -> ListZipper a
move (-1) (ListZipper (x : xs) y zs) = ListZipper xs x (y : zs)
move   1  (ListZipper xs y (z : zs)) = ListZipper (y : xs) z zs
move   0  lz                         = lz
move   n  lz | n < 0                 = move (n + 1) (move (-1) lz)
             | otherwise             = move (n - 1) (move (1) lz)

peek :: Int -> ListZipper a -> a
peek 0 lz             = focus lz
peek n lz | n < 0     = left lz !! abs n
          | otherwise = right lz !! n

apply :: (a -> a) -> ListZipper a -> ListZipper a
apply f (ListZipper xs y zs) = ListZipper xs (f y) zs

cut :: Int -> ListZipper a -> [a]
cut i (ListZipper xs y zs) = take i xs ++ [y] ++ take i zs
