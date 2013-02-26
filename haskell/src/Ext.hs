{-# LANGUAGE LambdaCase #-}
module Ext where

import Control.Arrow
import Data.Maybe

tryMaybe :: (a -> Maybe a) -> a -> a
tryMaybe f a = fromMaybe a (f a)

-- |Repeat a function a certain ammount of times
times :: (a -> a) -> Int -> a -> a
times f i a | i < 0     = error "negative number"
            | otherwise = go i a
  where
    go 0 b = b
    go j b = times f (j - 1) (f b)

-- |Repeat until a function returns the same function
whileModified :: Eq a => (a -> a) -> a -> a
whileModified f x = go x (f x)
  where
    go a b | a == b    = a
           | otherwise = go b (f b)

-- |Apply a function to a specific element in a list
mapIndex :: (a -> a) -> Int -> [a] -> [a]
mapIndex f i xs | i < 0     = error "negative index"
                | otherwise = go i xs
  where
    go 0 (y : ys) = f y : ys
    go j (y : ys) = y : mapIndex f (j - 1) ys
    go _ _        = error "list to short"

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
