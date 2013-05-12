{-# LANGUAGE BangPatterns #-}
module Brainfuck.Utility where

import Control.Arrow
import Control.Monad

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
