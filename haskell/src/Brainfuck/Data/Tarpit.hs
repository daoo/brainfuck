{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Tarpit where

import Brainfuck.Data.Expr
import Data.Monoid

data Function = Assign Int Expr | Shift Int | PutChar Expr | GetChar Int
  deriving (Eq, Show)

data Control = Forever | Once | Never | If Expr | While Expr
  deriving (Eq, Show)

data Tarpit = Nop
            | Instruction Function Tarpit
            | Flow Control Tarpit Tarpit
  deriving (Eq, Show)

instance Monoid Tarpit where
  mempty = Nop

  mappend a b = case a of
    Nop                  -> b
    Instruction fun next -> Instruction fun (mappend next b)
    Flow ctrl inner next -> Flow ctrl inner (mappend next b)

initTarpit :: Tarpit -> Tarpit
initTarpit = \case
  Nop               -> Nop
  Instruction _ Nop -> Nop
  Flow _ Nop _      -> Nop
  Flow _ _ Nop      -> Nop

  Instruction fun next -> Instruction fun (initTarpit next)
  Flow ctrl inner next -> Flow ctrl (initTarpit inner) (initTarpit next)

initsTarpit :: Tarpit -> [Tarpit]
initsTarpit = \case
  Nop -> []
  x   -> let x' = initTarpit x in x' : initsTarpit x'

mapTarpit :: (Function -> Function) -> (Control -> Control) -> Tarpit -> Tarpit
mapTarpit f g = \case
  Nop                  -> Nop
  Flow ctrl inner next -> Flow (g ctrl) (mapTarpit f g inner) (mapTarpit f g next)
  Instruction fun next -> Instruction (f fun) (mapTarpit f g next)
