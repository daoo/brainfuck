module Brainfuck.Interpreter.State where

import Data.Char
import Data.Sequence
import Data.Word

type Cell   = Word8
type Memory = ([Cell], [Cell])

data State = State {
  input :: [Cell],
  output :: Seq Cell,
  memory :: Memory
} deriving Show

chrCell :: Cell -> Char
chrCell = chr . fromIntegral

ordCell :: Char -> Cell
ordCell = fromIntegral . ord

brainfuckChars :: [Char]
brainfuckChars = "-+<>[].,"

newMemory :: Memory
newMemory = (zeros, zeros)
  where
    zeros = iterate id 0

newState :: String -> State
newState inp = State (map ordCell inp) empty newMemory

