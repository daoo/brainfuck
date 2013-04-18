module Brainfuck.Data.IOMemory
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
import Data.Array.IO
import Prelude hiding (read)

data Memory = Memory { ptr :: Int, array :: IOArray Int Int }

type IOMemory a = StateT Memory IO a

newMemory :: Int -> IO Memory
newMemory i = Memory <$> pure 0 <*> newArray (0, i) 0

read :: Int -> IOMemory Int
read d = get >>= (\m -> lift $ readArray (array m) (ptr m + d))

write :: Int -> Int -> IOMemory ()
write d v = get >>= (\m -> lift $ writeArray (array m) (ptr m + d) v)

shift :: Int -> IOMemory ()
shift d = modify (\m -> m { ptr = ptr m + d })

eval :: Expr -> IOMemory Int
eval = go 0
  where
    go acc (Const c)    = return $ acc + c
    go acc (Var n d xs) = (*n) <$> read d >>= (\acc' -> go (acc + acc') xs)

when :: Expr -> IOMemory () -> IOMemory ()
when e f = do
  x <- eval e
  unless (x == 0) f

while :: Expr -> IOMemory () -> IOMemory ()
while e f = when e (f >> while e f)

runMemory :: Int -> IOMemory () -> IO ()
runMemory size m = newMemory size >>= (void . runStateT m)
