{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Brainfuck where

import Data.List
import Test.QuickCheck

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

data Brainfuck = Repeat [Brainfuck]
               | Token Token
  deriving Eq

toChar :: Token -> Char
toChar = \case
  Plus       -> '+'
  Minus      -> '-'
  ShiftLeft  -> '<'
  ShiftRight -> '>'
  Input      -> ','
  Output     -> '.'

instance Show Token where
  show t = [toChar t]

instance Show Brainfuck where
  show = \case
    Repeat bf -> showString "[" $ showList bf "]"
    Token t   -> show t

  showList = flip (foldr shows)

instance Arbitrary Token where
  arbitrary = oneof $ map return [Plus, Minus, ShiftRight, ShiftLeft, Input, Output]

instance Arbitrary Brainfuck where
  arbitrary = Token `fmap` arbitrary

  shrink (Repeat bf) = map Repeat $ tails bf
  shrink _           = []
