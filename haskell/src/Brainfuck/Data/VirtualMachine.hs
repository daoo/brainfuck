module Brainfuck.Data.VirtualMachine where

import Brainfuck.Data.Expr
import Control.Monad (unless)
import Prelude hiding (read)

class (Functor vm, Monad vm) => VirtualMachine vm where
  shift :: Int -> vm ()
  read :: Int -> vm Int
  write :: Int -> Int -> vm ()

  putchr :: Int -> vm ()
  getchr :: vm Int

{-# INLINE eval #-}
eval :: VirtualMachine vm => Expr -> vm Int
eval = foldExprM'
  (\acc a x -> (\n -> acc + a*n) <$> read x)
  (\acc n   -> return (acc+n))
  0

{-# INLINE set #-}
set :: VirtualMachine vm => Int -> Expr -> vm ()
set d e = eval e >>= write d

{-# INLINE put #-}
put :: VirtualMachine vm => Expr -> vm ()
put e = eval e >>= putchr

{-# INLINE get #-}
get :: VirtualMachine vm => Int -> vm ()
get d = getchr >>= write d

{-# INLINE when #-}
when :: VirtualMachine vm => Expr -> vm () -> vm ()
when e f = do
  x <- eval e
  unless (x == 0) f

while :: VirtualMachine vm => Expr -> vm () -> vm ()
while e f = when e (f >> while e f)
