{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Brainfuck
  ( Token(..)
  , Brainfuck(..)
  ) where

import Control.Applicative
import Test.QuickCheck

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

instance Arbitrary Token where
  arbitrary = elements [Plus, Minus, ShiftRight, ShiftLeft, Input, Output]

data Brainfuck = Token !Token Brainfuck
               | Repeat Brainfuck Brainfuck
               | Nop
  deriving Eq

instance Arbitrary Brainfuck where
  arbitrary = sized go
    where
      go 0 = return Nop
      go n = frequency [ (1, Repeat <$> go (n-1) <*> go (n `div` 2))
                       , (7, Token <$> arbitrary <*> go (n-1))
                       ]

instance Show Token where
  show = \case
    Plus       -> "+"
    Minus      -> "-"
    ShiftLeft  -> "<"
    ShiftRight -> ">"
    Input      -> ","
    Output     -> "."

instance Show Brainfuck where
  show = \case
    Token t next      -> shows t $ show next
    Repeat inner next -> showChar '[' $ shows inner $ showChar ']' $ show next
    Nop               -> ""

  showList = flip (foldr shows)
