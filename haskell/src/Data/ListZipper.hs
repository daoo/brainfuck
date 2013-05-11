{-# LANGUAGE BangPatterns #-}
module Data.ListZipper where

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

-- |Return the length of the list zipper.
-- That is the length of the left and the right plus 1 (for the focus).
size :: ListZipper a -> Int
size (ListZipper xs _ zs) = length xs + 1 + length zs

-- |Move list zipper to the left.
-- Does nothing if the left is empty.
moveLeft :: ListZipper a -> ListZipper a
moveLeft (ListZipper (x:xs) y zs)  = ListZipper xs x (y:zs)
moveLeft ls                        = ls

-- |Move list zipper to the right.
-- Does nothing if the right is empty.
moveRight :: ListZipper a -> ListZipper a
moveRight (ListZipper xs y (z:zs)) = ListZipper (y:xs) z zs
moveRight ls                       = ls

-- |Move list zipper n steps.
-- Negative number means to the left, positive number means to the right, zero
-- means no moves.
move :: Int -> ListZipper a -> ListZipper a
move !n lz = case compare n 0 of
  EQ -> lz
  LT -> move (n + 1) (moveLeft lz)
  GT -> move (n - 1) (moveRight lz)

-- |Fetch an item from the zipper by index.
peek :: Int -> ListZipper a -> a
peek !n lz = case compare n 0 of
  EQ -> focus lz
  LT -> left lz !! (abs n - 1)
  GT -> right lz !! (n - 1)

-- |Apply a function tho the focus of the zipper.
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
cut :: Int -> ListZipper a -> [a]
cut i (ListZipper xs y zs) = take i xs ++ [y] ++ take i zs
