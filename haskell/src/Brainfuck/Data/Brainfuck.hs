{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Brainfuck where

import Control.Applicative
import Test.QuickCheck

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

data Brainfuck = Nop
               | Repeat Brainfuck Brainfuck
               | Token !Token Brainfuck
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
    Nop               -> ""
    Repeat inner next -> showString "[" $ shows inner $ showString "]" $ show next
    Token t next      -> shows t $ show next

  showList = flip (foldr shows)

instance Arbitrary Token where
  arbitrary = oneof $ map return [Plus, Minus, ShiftRight, ShiftLeft, Input, Output]

instance Arbitrary Brainfuck where
  arbitrary = Token <$> arbitrary <*> arbitrary

  shrink (Repeat inner next) = map (`Repeat` next) (shrink inner)
  shrink _                   = []
