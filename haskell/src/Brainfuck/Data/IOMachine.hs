{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
module Brainfuck.Data.IOMachine
  ( shift
  , read
  , write
  , putchr
  , getchr

  , eval
  , set
  , put
  , get
  , when
  , while

  , runMemory
  ) where

import Brainfuck.Data.VirtualMachine
import Control.Applicative hiding (Const)
import Control.Monad hiding (when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO
import Data.Char
import Prelude hiding (read)
import qualified Control.Monad.State.Strict as S

data Memory = Memory
  { ptr :: Int
  , array :: IOArray Int Int
  }

type IOMachine = S.StateT Memory IO

instance VirtualMachine IOMachine where
  {-# INLINE shift #-}
  shift d = S.modify (\m -> m { ptr = ptr m + d })

  {-# INLINE read #-}
  read d = S.get >>= (\m -> S.lift $ unsafeRead (array m) (ptr m + d))

  {-# INLINE write #-}
  write d v = S.get >>= (\m -> S.lift $ unsafeWrite (array m) (ptr m + d) v)

  {-# INLINE putchr #-}
  putchr = S.lift . putChar . chr

  {-# INLINE getchr #-}
  getchr d = ord <$> S.lift getChar >>= write d

{-# INLINE newMemory #-}
newMemory :: Int -> IO Memory
newMemory i = Memory <$> pure 0 <*> newArray (0, i) 0

{-# INLINE runMemory #-}
runMemory :: Int -> IOMachine () -> IO ()
runMemory size m = newMemory size >>= (void . S.runStateT m)
