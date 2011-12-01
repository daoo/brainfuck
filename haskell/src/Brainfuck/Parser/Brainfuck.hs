module Brainfuck.Parser.Brainfuck where

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

data Brainfuck = BFLoop [Brainfuck]
               | BFToken Token
  deriving Eq

instance Show Token where
  show Plus       = "+"
  show Minus      = "-"
  show ShiftLeft  = "<"
  show ShiftRight = ">"
  show Input      = ","
  show Output     = "."

instance Show Brainfuck where
  show (BFLoop bf) = concat ["[", concatMap show bf, "]"]
  show (BFToken t) = show t

