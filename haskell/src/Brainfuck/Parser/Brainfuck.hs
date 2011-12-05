module Brainfuck.Parser.Brainfuck where

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

data Brainfuck = Loop [Brainfuck]
               | Token Token
  deriving Eq

instance Show Token where
  show Plus       = "+"
  show Minus      = "-"
  show ShiftLeft  = "<"
  show ShiftRight = ">"
  show Input      = ","
  show Output     = "."

instance Show Brainfuck where
  show (Loop bf) = concat ["[", concatMap show bf, "]"]
  show (Token t) = show t

