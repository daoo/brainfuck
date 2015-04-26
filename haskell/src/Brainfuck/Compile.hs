{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compile (compile, decompile) where

import Brainfuck.Data.Brainfuck as BF
import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit as Tarpit

compile :: Brainfuck -> Tarpit
compile = \case
  BF.Nop            -> Tarpit.Nop
  Repeat inner next -> Flow (While (Variable1 0)) (compile inner) (compile next)
  Token t next      -> Instruction (token t) (compile next)
  where
    token = \case
      Plus       -> Assign 0 $ Add 0 1
      Minus      -> Assign 0 $ Add 0 (-1)
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ Variable1 0
      Input      -> GetChar 0

decompile :: Tarpit -> Maybe Brainfuck
decompile = \case
  Tarpit.Nop ->
    Just BF.Nop

  Instruction fun next ->
    tokenize fun <*> decompile next

  Flow (While (Variable1 0)) inner next ->
    Repeat <$> decompile inner <*> decompile next

  _ -> Nothing

  where
    tokenize = \case
      Assign 0 (Add 0 1)    -> Just (Token Plus)
      Assign 0 (Add 0 (-1)) -> Just (Token Minus)
      Shift 1               -> Just (Token ShiftRight)
      Shift (-1)            -> Just (Token ShiftLeft)
      PutChar (Variable1 0) -> Just (Token Output)
      GetChar 0             -> Just (Token Input)

      _ -> Nothing
