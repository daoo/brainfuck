module Brainfuck.Interpreter.State where

import Data.Char
import Data.List
import Data.Sequence

import Brainfuck.Ext

data State a = State {
  getInput :: [a],
  getOutput :: Seq a,
  getMemory :: ([a], [a])
} deriving (Show, Eq)

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

current :: ([a], [a]) -> a
current = head . fst

offset :: (Integral a) => a -> ([b], [b]) -> b
offset i tup | i < 0     = (`genericIndex` (abs i - 1)) $ snd tup
             | otherwise = (`genericIndex` i) $ fst tup

modify :: (Integral b) => (a -> a) -> b -> ([a], [a]) -> ([a], [a])
modify f i tup | i < 0     = mapSnd (mapIndex f (abs i - 1)) tup
               | otherwise = mapFst (mapIndex f i) tup
