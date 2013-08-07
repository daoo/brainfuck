module Brainfuck.Data.Tarpit
  ( Function (..)
  , Control (..)
  , Tarpit (..)
  ) where

import Brainfuck.Data.Expr
import Data.Monoid

data Function = Assign Int (Expr Int) | Shift Int | PutChar (Expr Int) | GetChar Int
  deriving Show

data Control = If (Expr Int) | While (Expr Int)
  deriving Show

data Tarpit = Nop
            | Instruction Function Tarpit
            | Flow Control Tarpit Tarpit
  deriving Show

instance Monoid Tarpit where
  mempty = Nop

  mappend a b = case a of
    Nop                  -> b
    Instruction fun next -> Instruction fun (mappend next b)
    Flow ctrl inner next -> Flow ctrl inner (mappend next b)
