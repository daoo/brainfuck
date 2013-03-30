{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compile (compile, decompile) where

import Brainfuck.Data.Brainfuck as BF
import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit as Tarpit

compile :: Brainfuck -> Tarpit
compile = \case
  BF.Nop            -> Tarpit.Nop
  Repeat inner next -> Flow (While (variable 0)) (compile inner) (compile next)
  Token t next      -> Instruction (token t) (compile next)
  where
    token = \case
      Plus       -> Assign 0 $ variable 0 `add` constant 1
      Minus      -> Assign 0 $ variable 0 `add` constant (-1)
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ variable 0
      Input      -> GetChar 0

decompile :: Tarpit -> Brainfuck
decompile = \case
  Tarpit.Nop -> BF.Nop

  Instruction fun next -> tokenize fun (decompile next)

  Flow (While (Expr 0 [(Mult 1, Var 0)])) inner next -> Repeat (decompile inner) (decompile next)

  tarpit -> error $ "Brainfuck.Compile.decompile unsupported: " ++ show tarpit

  where
    tokenize = \case
      Assign 0 (Expr 1 [(Mult 1, Var 0)])    -> Token Plus
      Assign 0 (Expr (-1) [(Mult 1, Var 0)]) -> Token Minus
      Shift 1                                -> Token ShiftRight
      Shift (-1)                             -> Token ShiftLeft
      PutChar (Expr 0 [(Mult 1, Var 0)])     -> Token Output
      GetChar 0                              -> Token Input

      fun -> error $ "Brainfuck.Compile.decompile.tokenize unsupported: " ++ show fun
