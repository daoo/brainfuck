{-# LANGUAGE GADTs #-}
module Brainfuck.Data.Tarpit
  ( Function (..)
  , Control (..)
  , Tarpit (..)
  , (&=)
  , (&=!)
  ) where

import Brainfuck.Data.Expr
import Data.Monoid

data Function where
  Assign  :: Int -> Expr -> Function
  Shift   :: Int -> Function
  PutChar :: Expr -> Function
  GetChar :: Int -> Function
  deriving Show

data Control where
  If    :: Expr -> Control
  While :: Expr -> Control
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

(&=!) :: Int -> Expr -> Function
(&=!) = Assign
