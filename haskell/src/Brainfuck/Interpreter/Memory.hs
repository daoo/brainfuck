module Brainfuck.Interpreter.Memory where

import Data.Char
import Data.Word

type Cell   = Word8
type Memory = ([Cell], [Cell])
type Input  = [Cell]
type Output = [Cell]
type State  = (Input, Output, Memory)

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

newState :: State
newState = ([], [], newMemory)
