module Brainfuck.Utility where

{-# INLINE thrd #-}
thrd :: (a, b, c) -> c
thrd (_, _, z) = z
