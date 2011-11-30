module Brainfuck.Parse.Brainfuck where

data Token = Plus | Minus | Next | Previous | Input | Output
  deriving Show

data Brainfuck = Loop [Brainfuck]
               | Token Token
  deriving Show

