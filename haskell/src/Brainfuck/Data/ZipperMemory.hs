module Brainfuck.Data.ZipperMemory
  ( read
  , write
  , shift
  , eval
  , when
  , while
  , runMemory
  ) where

import Brainfuck.Data.Expr hiding (eval)
import Control.Applicative hiding (Const)
import Control.Monad.State.Strict hiding (when)
import Data.ListZipper
import Prelude hiding (read)

type Memory = ListZipper Int

type Machine a = StateT Memory IO a

newMemory :: Memory
newMemory = ListZipper (repeat 0) 0 (repeat 0)

read :: Int -> Machine Int
read d = peek d `fmap` get

write :: Int -> Int -> Machine ()
write d v = modify $ applyAt (const v) d

shift :: Int -> Machine ()
shift d = modify $ move d

eval :: Expr -> Machine Int
eval e = fmap (\m -> go m 0 e) get
  where
    go _ acc (Const c)    = acc + c
    go m acc (Var n d xs) = go m (acc + n * peek d m) xs

when :: Expr -> Machine () -> Machine ()
when e f = do
  x <- eval e
  unless (x == 0) f

while :: Expr -> Machine () -> Machine ()
while e f = when e (f >> while e f)

runMemory :: Int -> Machine () -> IO ()
runMemory _ m = void $ runStateT m newMemory
