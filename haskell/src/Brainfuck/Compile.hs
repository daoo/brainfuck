{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compile (compile, decompile) where

import Brainfuck.Data.AST
import Brainfuck.Data.Brainfuck
import Brainfuck.Data.Expr

compile :: [Brainfuck] -> AST
compile = \case
  []             -> Nop
  Repeat ys : xs -> Flow (While (Var 0)) (compile ys) (compile xs)
  Token t : xs   -> Instruction (token t) (compile xs)
  where
    token = \case
      Plus       -> Assign 0 $ Const 1 `Add` Var 0
      Minus      -> Assign 0 $ Const (-1) `Add` Var 0
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ Var 0
      Input      -> GetChar 0

decompile :: AST -> [Brainfuck]
decompile = \case
  Nop                             -> []
  Instruction fun next            -> tokenize fun : decompile next
  Flow (While (Var 0)) inner next -> Repeat (decompile inner) : decompile next

  _ -> error "unsupported by decompile"
  where
    tokenize = \case
      Assign 0 (Add (Const 1) (Var 0))    -> Token $ Plus
      Assign 0 (Add (Const (-1)) (Var 0)) -> Token $ Minus

      Shift 1         -> Token $ ShiftRight
      Shift (-1)      -> Token $ ShiftLeft
      PutChar (Var 0) -> Token $ Output
      GetChar 0       -> Token $ Input

      _ -> error "unsupported by decompile"
