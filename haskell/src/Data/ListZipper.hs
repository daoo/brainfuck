{-# LANGUAGE BangPatterns #-}
module Data.ListZipper
  ( ListZipper(..)
  , size

  , moveLeft
  , moveRight
  , move

  , peek
  , apply
  , applyAt

  , cut
  , toList
  ) where

import Test.QuickCheck

-- |List zipper data structure.
-- Tuned for performance, thus does not throw any errors if you try to access
-- elements out of range etc.
-- The zipper can be indexed as follows:
--    * 0 is the focus
--    * negative numbers refers to the left list
--    * positive numbers refers to the right list
-- The left and right indices are 1-based.
data ListZipper a = ListZipper
  { left :: [a]
  , focus :: !a
  , right :: [a]
  } deriving (Show, Eq)

toList :: ListZipper a -> [a]
toList (ListZipper l f r) = reverse l ++ [f] ++ r

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary = ListZipper <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (ListZipper xs y zs) =
    [ ListZipper xs' y' zs' | xs' <- shrink xs, y' <- shrink y, zs' <- shrink zs ]

-- |Return the length of the list zipper.
-- That is the length of the left and the right plus 1 (for the focus).
--
-- prop> size z == length (toList z)
size :: ListZipper a -> Int
size (ListZipper xs _ zs) = length xs + 1 + length zs

-- |Move list zipper to the left. Does nothing if the left is empty.
--
-- prop> toList (moveLeft z) == toList z
moveLeft :: ListZipper a -> ListZipper a
moveLeft (ListZipper (x:xs) y zs)  = ListZipper xs x (y:zs)
moveLeft ls                        = ls

-- |Move list zipper to the right. Does nothing if the right is empty.
--
-- prop> toList (moveRight z) == toList z
moveRight :: ListZipper a -> ListZipper a
moveRight (ListZipper xs y (z:zs)) = ListZipper (y:xs) z zs
moveRight ls                       = ls

-- |Move list zipper n steps. Negative number means to the left, positive
-- number means to the right, zero means no moves.
--
-- prop> toList (move n z) == toList z
move :: Int -> ListZipper a -> ListZipper a
move !n lz = case compare n 0 of
  EQ -> lz
  LT -> move (n+1) (moveLeft lz)
  GT -> move (n-1) (moveRight lz)

-- |Fetch an item from the zipper by index.
--
-- prop> peek 0 (ListZipper l f r) == (f :: Int)
peek :: Int -> ListZipper a -> a
peek !n lz = case compare n 0 of
  EQ -> focus lz
  LT -> left lz !! (abs n - 1)
  GT -> right lz !! (n-1)

-- |Apply a function to the focus of the zipper.
--
-- prop> apply id z == z
apply :: (a -> a) -> ListZipper a -> ListZipper a
apply f (ListZipper xs y zs) = ListZipper xs (f y) zs

-- |Apply a function at some index in the zipper.
applyAt :: (a -> a) -> Int -> ListZipper a -> ListZipper a
applyAt f n (ListZipper xs y zs) = case compare n 0 of
  EQ -> ListZipper xs (f y) zs
  LT -> ListZipper (go (abs n - 1) xs) y zs
  GT -> ListZipper xs y (go (n - 1) zs)

  where
    go  _ []       = []
    go  0 (a:as) = f a : as
    go !i (a:as) = a : go (i-1) as

-- |Take from left and right and add the focus.
--
-- prop> cut 0 (ListZipper l f r) == [f]
cut :: Int -> ListZipper a -> [a]
cut i (ListZipper xs y zs) = take i xs ++ [y] ++ take i zs
