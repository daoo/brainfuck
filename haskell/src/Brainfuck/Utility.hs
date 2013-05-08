{-# LANGUAGE BangPatterns #-}
module Brainfuck.Utility where

import Control.Arrow
import Control.Monad

-- |Repeat a function a certain ammount of times
times :: (a -> a) -> Int -> a -> a
times f !i a | i < 0     = error "negative number"
             | otherwise = go i a
  where
    go  0 !b = b
    go !j !b = times f (j - 1) (f b)

-- |Apply a function to a specific element in a list
mapIndex :: (a -> a) -> Int -> [a] -> [a]
mapIndex f !i xs | i < 0     = error "negative index"
                 | otherwise = go i xs
  where
    go  0 (y : ys) = f y : ys
    go !j (y : ys) = y : go (j - 1) ys
    go  _ _        = error "list to short"

{-# INLINE thrd #-}
thrd :: (a, b, c) -> c
thrd (_, _, z) = z

{-# INLINE mapFst #-}
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst = (*** id)

{-# INLINE mapSnd #-}
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd = (id ***)

{-# INLINE when' #-}
when' :: Monad m => m Bool -> m () -> m ()
when' f g = f >>= (`when` g)

while :: Monad m => m Bool -> m a -> m ()
while f g = when' f (g >> while f g)
