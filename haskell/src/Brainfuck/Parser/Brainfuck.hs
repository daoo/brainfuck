module Brainfuck.Parser.Brainfuck where

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

data Brainfuck = Loop [Brainfuck]
               | Token Token
  deriving Eq

toChar :: Token -> Char
toChar Plus       = '+'
toChar Minus      = '-'
toChar ShiftLeft  = '<'
toChar ShiftRight = '>'
toChar Input      = ','
toChar Output     = '.'

instance Show Token where
  show t = [toChar t]

instance Show Brainfuck where
  show (Loop bf) = concat ["[", concatMap show bf, "]"]
  show (Token t) = show t

  showList bf = (++) (concatMap show bf)

