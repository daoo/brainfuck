{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compile (compile, decompile) where

import Brainfuck.Data.Brainfuck as BF
import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit as Tarpit

compile :: Brainfuck -> Tarpit
compile = \case
  BF.Nop            -> Tarpit.Nop
  Repeat inner next -> Flow (While (Var 1 0 (Const 0))) (compile inner) (compile next)
  Token t next      -> Instruction (token t) (compile next)
  where
    token = \case
      Plus       -> Assign 0 $ Var 1 0 (Const 1)
      Minus      -> Assign 0 $ Var 1 0 (Const (-1))
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ Var 1 0 (Const 0)
      Input      -> GetChar 0

decompile :: Tarpit -> Brainfuck
decompile = \case
  Tarpit.Nop -> BF.Nop

  Instruction fun next -> tokenize fun (decompile next)

  Flow (While (Var 1 0 (Const 0))) inner next -> Repeat (decompile inner) (decompile next)

  _ -> undefined

  where
    tokenize = \case
      Assign 0 (Var 1 0 (Const 1))    -> Token Plus
      Assign 0 (Var 1 0 (Const (-1))) -> Token Minus
      Shift 1                         -> Token ShiftRight
      Shift (-1)                      -> Token ShiftLeft
      PutChar (Var 1 0 (Const 0))     -> Token Output
      GetChar 0                       -> Token Input

      _ -> undefined
