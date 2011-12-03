module Brainfuck.Interpreter.State where

import Data.Char
import Data.Sequence
import Data.Word

data State a = State {
  input :: [a],
  output :: Seq a,
  memory :: ([a], [a])
} deriving Show

chrIntegral :: (Integral a) => a -> Char
chrIntegral = chr . fromIntegral

ordIntegral :: (Integral a) => Char -> a
ordIntegral = fromIntegral . ord

newMemory :: (Integral a) => ([a], [a])
newMemory = (zeros, zeros)
  where
    zeros = iterate id 0

newState :: (Integral a) => String -> State a
newState inp = State (map ordIntegral inp) empty newMemory

