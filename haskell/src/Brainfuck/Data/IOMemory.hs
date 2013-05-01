{-# LANGUAGE BangPatterns #-}
module Brainfuck.Data.IOMemory
  ( runMemory
  , write
  , putchr
  , getchr
  , set
  , shift
  , when
  , while
  ) where

import Brainfuck.Data.Expr hiding (eval)
import Control.Applicative hiding (Const)
import Control.Monad hiding (when)
import Data.Array.IO
import Data.Char
import Prelude hiding (read)
import qualified Control.Monad.State.Strict as S

data Memory = Memory { ptr :: Int, array :: IOArray Int Int }

type IOMemory a = S.StateT Memory IO a

{-# INLINE newMemory #-}
newMemory :: Int -> IO Memory
newMemory i = Memory <$> pure 0 <*> newArray (0, i) 0

{-# INLINE read #-}
read :: Int -> IOMemory Int
read d = S.get >>= (\m -> S.lift $ readArray (array m) (ptr m + d))

{-# INLINE write #-}
write :: Int -> Int -> IOMemory ()
write d v = S.get >>= (\m -> S.lift $ writeArray (array m) (ptr m + d) v)

{-# INLINE eval #-}
eval :: Expr -> IOMemory Int
eval = go 0
  where
    go !acc (Const c)    = return $ acc + c
    go !acc (Var n d xs) = (*n) <$> read d >>= (\acc' -> go (acc + acc') xs)

{-# INLINE putchr #-}
putchr :: Expr -> IOMemory ()
putchr e = eval e >>= S.lift . putChar . chr

{-# INLINE getchr #-}
getchr :: Int -> IOMemory ()
getchr d = ord <$> S.lift getChar >>= write d

{-# INLINE set #-}
set :: Int -> Expr -> IOMemory ()
set d e = eval e >>= write d

{-# INLINE shift #-}
shift :: Int -> IOMemory ()
shift d = S.modify (\m -> m { ptr = ptr m + d })

{-# INLINE when #-}
when :: Expr -> IOMemory () -> IOMemory ()
when e f = do
  x <- eval e
  unless (x == 0) f

while :: Expr -> IOMemory () -> IOMemory ()
while e f = when e (f >> while e f)

{-# INLINE runMemory #-}
runMemory :: Int -> IOMemory () -> IO ()
runMemory size m = newMemory size >>= (void . S.runStateT m)
