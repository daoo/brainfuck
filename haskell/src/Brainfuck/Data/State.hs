module Brainfuck.Data.State where

import Data.Char
import Data.ListZipper
import Data.Sequence

data State a = State {
  getInput :: [a],
  getOutput :: Seq a,
  getMemory :: ListZipper a
} deriving (Show, Eq)

chrIntegral :: (Integral a) => a -> Char
chrIntegral = chr . fromIntegral

ordIntegral :: (Integral a) => Char -> a
ordIntegral = fromIntegral . ord

newMemory :: (Integral a) => ListZipper a
newMemory = let zeros = repeat 0 in ListZipper zeros 0 zeros

newState :: (Integral a) => String -> State a
newState inp = State (map ordIntegral inp) empty newMemory
