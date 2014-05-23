{-# LANGUAGE GADTs #-}
module Brainfuck.Data.Tarpit
  ( Function (..)
  , Control (..)
  , Tarpit (..)
  , IntExpr
  , (&=)
  , (&=!)
  ) where

import Brainfuck.Data.Expr
import Data.Monoid

type IntExpr = Expr Int Int

data Function where
  Assign  :: Int -> IntExpr -> Function
  Shift   :: Int -> Function
  PutChar :: IntExpr -> Function
  GetChar :: Int -> Function
  deriving Show

data Control where
  If    :: IntExpr -> Control
  While :: IntExpr -> Control
  deriving Show

data Tarpit where
  Instruction :: Function -> Tarpit -> Tarpit
  Flow        :: Control -> Tarpit -> Tarpit -> Tarpit
  Nop         :: Tarpit
  deriving Show

instance Monoid Tarpit where
  mempty = Nop

  mappend a b = case a of
    Nop                  -> b
    Instruction fun next -> Instruction fun (mappend next b)
    Flow ctrl inner next -> Flow ctrl inner (mappend next b)

(&=) :: Int -> Int -> Function
d &= c = Assign d (econst c)

(&=!) :: Int -> IntExpr -> Function
(&=!) = Assign
