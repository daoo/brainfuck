{-# LANGUAGE BangPatterns #-}
module Data.ListZipper where

data ListZipper a = ListZipper
  { left :: [a]
  , focus :: !a
  , right :: [a]
  } deriving (Show, Eq)

size :: ListZipper a -> Int
size (ListZipper xs _ zs) = length xs + 1 + length zs

moveLeft, moveRight :: ListZipper a -> ListZipper a
moveLeft (ListZipper (x : xs) y zs) = ListZipper xs x (y : zs)
moveLeft _                          = error "moveLeft: left is empty"
moveRight (ListZipper xs y (z :zs)) = ListZipper (y : xs) z zs
moveRight _                         = error "moveRight: right is empty"

move :: Int -> ListZipper a -> ListZipper a
move !n lz = case compare n 0 of
  EQ -> lz
  LT -> move (n + 1) (moveLeft lz)
  GT -> move (n - 1) (moveRight lz)

peek :: Int -> ListZipper a -> a
peek !n lz = case compare n 0 of
  EQ -> focus lz
  LT -> left lz !! (abs n - 1)
  GT -> right lz !! (n - 1)

apply :: (a -> a) -> ListZipper a -> ListZipper a
apply f (ListZipper xs y zs) = ListZipper xs (f y) zs

cut :: Int -> ListZipper a -> [a]
cut i (ListZipper xs y zs) = take i xs ++ [y] ++ take i zs

applyAt :: (a -> a) -> Int -> ListZipper a -> ListZipper a
applyAt f n (ListZipper xs y zs) = case compare n 0 of
  EQ -> ListZipper xs (f y) zs
  LT -> ListZipper (go (abs n - 1) xs) y zs
  GT -> ListZipper xs y (go (n - 1) zs)

  where
    go  0 (a : as) = f a : as
    go !j (a : as) = a : go (j - 1) as
    go  _ _        = error "list to short"

takeBoth :: Int -> ListZipper a -> [a]
takeBoth i (ListZipper xs y zs) = take i xs ++ [y] ++ take i zs
