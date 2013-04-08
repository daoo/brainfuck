module Brainfuck.Data.Tarpit
  ( Function (..)
  , Control (..)
  , Tarpit (..)
  ) where

import Brainfuck.Data.Expr
import Data.Monoid

data Function = Assign Int Expr | Shift Int | PutChar Expr | GetChar Int
  deriving Show

data Control = Forever | Once | Never | If Expr | While Expr
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
