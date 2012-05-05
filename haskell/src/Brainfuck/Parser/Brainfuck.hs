module Brainfuck.Parser.Brainfuck where

import Test.QuickCheck

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

data Brainfuck = Repeat [Brainfuck]
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
  show (Repeat bf) = concat ["[", concatMap show bf, "]"]
  show (Token t)   = show t

  showList bf = (++) (concatMap show bf)

instance Arbitrary Token where
  arbitrary = oneof $ map return [Plus, Minus, ShiftRight, ShiftLeft, Input, Output]

instance Arbitrary Brainfuck where
  arbitrary = Token `fmap` arbitrary

