{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Brainfuck.Data.ZipperMachine
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

  , Input
  , Output
  , MachineState(..)
  , runMemory
  ) where

import Brainfuck.Data.VirtualMachine
import Data.ListZipper
import Prelude hiding (read)
import qualified Control.Monad.State.Strict as ST
import qualified Data.Sequence as S

type Input  = [Int]
type Output = S.Seq Int
type Memory = ListZipper Int

data MachineState = MachineState
  { minput :: Input
  , moutput :: Output
  , mmemory :: Memory
  }

type ZipperMachine = ST.State MachineState

instance VirtualMachine ZipperMachine where
  shift d = ST.modify $ \s -> s { mmemory = move d (mmemory s) }

  read d = (peek d . mmemory) `fmap` ST.get

  write d v = ST.modify $ \s -> s { mmemory = applyAt (const v) d (mmemory s) }

  getchr = do
    s <- ST.get
    let (x:xs) = minput s
    ST.put $ s { minput = xs }
    return x

  putchr x = ST.modify $ \m -> m { moutput = moutput m S.|> x }

new :: [Int] -> MachineState
new inp = MachineState
  { minput = inp
  , moutput = S.empty
  , mmemory = ListZipper (repeat 0) 0 (repeat 0)
  }

runMemory :: [Int] -> ZipperMachine () -> MachineState
runMemory inp m = ST.execState m (new inp)
