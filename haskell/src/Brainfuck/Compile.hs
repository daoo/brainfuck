{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compile (compile, decompile) where

import Brainfuck.Data.AST
import Brainfuck.Data.Brainfuck
import Brainfuck.Data.Expr

compile :: [Brainfuck] -> AST
compile = \case
  []             -> Nop
  Repeat ys : xs -> Flow (While (Value (Get 0))) (compile ys) (compile xs)
  Token t : xs   -> Instruction (token t) (compile xs)
  where
    token = \case
      Plus       -> Set 0 $ mkInt 1 `add` mkGet 0
      Minus      -> Set 0 $ mkInt (-1) `add` mkGet 0
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ mkGet 0
      Input      -> GetChar 0

decompile :: AST -> [Brainfuck]
decompile = \case
  Nop                                     -> []
  Instruction fun next                    -> tokenize fun : decompile next
  Flow (While (Value (Get 0))) inner next -> Repeat (decompile inner) : decompile next

  _ -> error "unsupported by decompile"
  where
    tokenize = \case
      Set 0 (OperateBinary Add (Value (Const 1)) (Value (Get 0)))    -> Token $ Plus
      Set 0 (OperateBinary Add (Value (Const (-1))) (Value (Get 0))) -> Token $ Minus

      Shift 1                 -> Token $ ShiftRight
      Shift (-1)              -> Token $ ShiftLeft
      PutChar (Value (Get 0)) -> Token $ Output
      GetChar 0               -> Token $ Input

      _ -> error "unsupported by decompile"
