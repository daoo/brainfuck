module Brainfuck.Utility where

import Control.Arrow

{-# INLINE thrd #-}
thrd :: (a, b, c) -> c
thrd (_, _, z) = z

{-# INLINE mapFst #-}
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst = (*** id)

{-# INLINE mapSnd #-}
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd = (id ***)
