module Brainfuck.Data.Tarpit
  ( Function (..)
  , Control (..)
  , Tarpit (..)
  , IntExpr
  ) where

import Brainfuck.Data.Expr
import Data.Monoid

type IntExpr = Expr Int Int

data Function = Assign Int IntExpr | Shift Int | PutChar IntExpr | GetChar Int
  deriving Show

data Control = If IntExpr | While IntExpr
  deriving Show

data Tarpit = Instruction Function Tarpit
            | Flow Control Tarpit Tarpit
            | Nop
  deriving Show

instance Monoid Tarpit where
  mempty = Nop

  mappend a b = case a of
    Nop                  -> b
    Instruction fun next -> Instruction fun (mappend next b)
    Flow ctrl inner next -> Flow ctrl inner (mappend next b)
