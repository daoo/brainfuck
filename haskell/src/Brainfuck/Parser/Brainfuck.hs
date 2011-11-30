module Brainfuck.Parser.Brainfuck where

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Show

data Brainfuck = BFLoop [Brainfuck]
               | BFToken Token
  deriving Show

