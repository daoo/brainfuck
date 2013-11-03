{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Brainfuck
  ( Token(..)
  , Brainfuck(..)
  , toChar
  ) where

data Token = Plus | Minus | ShiftRight | ShiftLeft | Input | Output
  deriving Eq

data Brainfuck = Token !Token Brainfuck
               | Repeat Brainfuck Brainfuck
               | Nop
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
    Token t next      -> shows t $ show next
    Repeat inner next -> showChar '[' $ shows inner $ showChar ']' $ show next
    Nop               -> ""

  showList = flip (foldr shows)
