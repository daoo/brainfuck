module Data.ListZipper where

import Brainfuck.Ext
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

size :: ListZipper a -> Int
size (ListZipper xs _ zs) = length xs + 1 + length zs

move :: Int -> ListZipper a -> ListZipper a
move (-1) (ListZipper (x : xs) y zs) = ListZipper xs x (y : zs)
move   1  (ListZipper xs y (z : zs)) = ListZipper (y : xs) z zs
move   n  lz = case n `compare` 0 of
  EQ -> lz
  LT -> move (n + 1) (move (-1) lz)
  GT -> move (n - 1) (move 1 lz)

peek :: Int -> ListZipper a -> a
peek 0 lz             = focus lz
peek n lz | n < 0     = left lz !! (abs n - 1)
          | otherwise = right lz !! (n - 1)

apply :: (a -> a) -> ListZipper a -> ListZipper a
apply f (ListZipper xs y zs) = ListZipper xs (f y) zs

cut :: Int -> ListZipper a -> [a]
cut i (ListZipper xs y zs) = take i xs ++ [y] ++ take i zs

applyAt :: (a -> a) -> Int -> ListZipper a -> ListZipper a
applyAt f 0 (ListZipper xs y zz)             = ListZipper xs (f y) zz
applyAt f n (ListZipper xs y zz) | n < 0     = ListZipper (mapIndex f (abs n - 1) xs) y zz
                                 | otherwise = ListZipper xs y (mapIndex f (n - 1) zz)
