{-# LANGUAGE BangPatterns #-}
module Brainfuck.Data.VirtualMachine where

import Brainfuck.Data.Expr hiding (eval)
import Control.Applicative ((<$>))
import Control.Monad (unless)
import Prelude hiding (read)

class (Functor vm, Monad vm) => VirtualMachine vm where
  shift :: Int -> vm ()
  read :: Int -> vm Int
  write :: Int -> Int -> vm ()

  putchr :: Int -> vm ()
  getchr :: Int -> vm ()

eval :: VirtualMachine vm => Expr -> vm Int
eval = go 0
  where
    go !acc (Const c)    = return $ acc + c
    go !acc (Var n d xs) = (*n) <$> read d >>= (\acc' -> go (acc + acc') xs)

set :: VirtualMachine vm => Int -> Expr -> vm ()
set d e = eval e >>= write d

put :: VirtualMachine vm => Expr -> vm ()
put e = eval e >>= putchr

get :: VirtualMachine vm => Int -> vm ()
get = getchr

when :: VirtualMachine vm => Expr -> vm () -> vm ()
when e f = do
  x <- eval e
  unless (x == 0) f

while :: VirtualMachine vm => Expr -> vm () -> vm ()
while e f = when e (f >> while e f)
