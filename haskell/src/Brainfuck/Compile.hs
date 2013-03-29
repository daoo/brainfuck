{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compile (compile, decompile) where

import Brainfuck.Data.Brainfuck as BF
import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit as Tarpit
import qualified Data.IntMap as M

compile :: Brainfuck -> Tarpit
compile = \case
  BF.Nop            -> Tarpit.Nop
  Repeat inner next -> Flow (While (variable 1 0)) (compile inner) (compile next)
  Token t next      -> Instruction (token t) (compile next)
  where
    token = \case
      Plus       -> Assign 0 $ constant 1 `add` variable 0 1
      Minus      -> Assign 0 $ constant (-1) `add` variable 0 1
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar $ variable 1 0
      Input      -> GetChar 0

decompile :: Tarpit -> Brainfuck
decompile = \case
  Tarpit.Nop                -> BF.Nop
  Instruction fun next      -> tokenize fun (decompile next)
  Flow (While e) inner next -> case varAnalysis e of

    Just (1, 0) -> Repeat (decompile inner) (decompile next)
    _           -> error "unsupported by decompile"

  _ -> error "unsupported by decompile"
  where
    tokenize = \case
      Assign 0 (1, v) -> case M.assocs v of

        [(1, 0)] -> Token Plus
        _        -> error "unsupported by decompile"

      Assign 0 (-1, v) -> case M.assocs v of

        [(1, 0)] -> Token Minus
        _        -> error "unsupported by decompile"

      Shift 1    -> Token ShiftRight
      Shift (-1) -> Token ShiftLeft

      PutChar e  -> case varAnalysis e of

        Just (1, 0) -> Token Output
        _           -> error "unsupported by decompile"

      GetChar 0  -> Token Input

      _ -> error "unsupported by decompile"
