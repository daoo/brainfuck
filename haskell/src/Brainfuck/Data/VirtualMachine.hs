{-# LANGUAGE BangPatterns #-}
module Brainfuck.Data.VirtualMachine where

import Brainfuck.Data.Expr
import Control.Monad (unless)
import Prelude hiding (read)

class (Monad vm) => VirtualMachine vm where
  shift :: Int -> vm ()
  read :: Int -> vm Int
  write :: Int -> Int -> vm ()

  putchr :: Int -> vm ()
  getchr :: vm Int

{-# INLINE eval #-}
eval :: VirtualMachine vm => Expr Int Int -> vm Int
eval = go 0
  where
    go !acc (Const c)    = return $ acc + c
    go !acc (Var n d xs) = read d >>= \x -> go (acc + x * n) xs

{-# INLINE set #-}
set :: VirtualMachine vm => Int -> Expr Int Int -> vm ()
set d e = eval e >>= write d

{-# INLINE put #-}
put :: VirtualMachine vm => Expr Int Int -> vm ()
put e = eval e >>= putchr

{-# INLINE get #-}
get :: VirtualMachine vm => Int -> vm ()
get d = getchr >>= write d

{-# INLINE when #-}
when :: VirtualMachine vm => Expr Int Int -> vm () -> vm ()
when e f = do
  x <- eval e
  unless (x == 0) f

while :: VirtualMachine vm => Expr Int Int -> vm () -> vm ()
while e f = when e (f >> while e f)