{-# LANGUAGE LambdaCase #-}
module Ext where

import Control.Arrow

-- |Repeat a function a certain ammount of times
times :: (a -> a) -> Int -> a -> a
times f i a = case compare i 0 of
  LT -> error "Negative number"
  EQ -> a
  GT -> times f (i - 1) (f a)

-- |Repeat until a function returns the same function
whileModified :: Eq a => (a -> a) -> a -> a
whileModified f x = go x (f x)
  where
    go a b | a == b    = a
           | otherwise = go b (f b)

-- |Apply a function to a specific element in a list
mapIndex :: (a -> a) -> Int -> [a] -> [a]
mapIndex f 0 (x : xs) = f x : xs
mapIndex f i (x : xs) = x : mapIndex f (i - 1) xs
mapIndex _ _ _        = error "Index out of range"

-- |Pipe a value through a list of functions
pipe :: [a -> a] -> a -> a
pipe = flip (foldr ($))

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst = (*** id)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd = (id ***)

-- |Like mapAccumL but drops Nothing from resulting list
mapAccumL' :: (acc -> x -> (acc, Maybe y)) -> acc -> [x] -> (acc, [y])
mapAccumL' _ acc []     = (acc, [])
mapAccumL' f acc (x:xs) = (acc'', maybe ys (:ys) y)
  where
    (acc', y)   = f acc x
    (acc'', ys) = mapAccumL' f acc' xs

tailp :: (a -> a -> a) -> t -> a -> a -> a
tailp f _ = f
