{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compiler.Brainfuck (compile, decompile) where

import Brainfuck.Data.Brainfuck
import Brainfuck.Data.Expr
import Brainfuck.Data.IL

compile :: [Brainfuck] -> [IL]
compile = \case
  []             -> []
  Repeat ys : xs -> While (Get 0) (compile ys) : compile xs
  Token t : xs   -> token t : compile xs
  where
    token = \case
      Plus       -> Set 0 $ Add (Get 0) (Const 1)
      Minus      -> Set 0 $ Add (Get 0) (Const (-1))
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ Get 0
      Input      -> GetChar 0

decompile :: [IL] -> [Brainfuck]
decompile []     = []
decompile (x:xs) = tokenize x : decompile xs
  where
    tokenize = \case
      While (Get 0) ys               -> Repeat (decompile ys)
      Set 0 (Get 0 `Add` Const 1)    -> Token $ Plus
      Set 0 (Get 0 `Add` Const (-1)) -> Token $ Minus
      Shift 1                        -> Token $ ShiftRight
      Shift (-1)                     -> Token $ ShiftLeft
      PutChar (Get 0)                -> Token $ Output
      GetChar 0                      -> Token $ Input

      _ -> error "Not supported by decompile"
