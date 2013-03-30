{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.WholeProgram where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import qualified Data.Set as S

-- |Inline initial zeroes
inlineZeros :: Tarpit -> Tarpit
inlineZeros = go S.empty
  where
    go :: S.Set Int -> Tarpit -> Tarpit
    go s = \case
      Instruction fun next -> case fun of

        Assign i e -> Instruction (Assign i (inl s e)) (go (S.insert i s) next)
        PutChar e  -> Instruction (PutChar (inl s e)) (go s next)
        GetChar d  -> Instruction fun (go (S.delete d s) next)
        Shift _    -> Instruction fun next

      ast -> ast

    inl :: S.Set Int -> Expr -> Expr
    inl s = filterVars (`S.member` s)

-- |Remove instructions from the end that does not performe any side effects
removeFromEnd :: Tarpit -> Tarpit
removeFromEnd = \case
  Nop                          -> Nop
  Instruction (Assign _ _) Nop -> Nop
  Instruction (Shift _) Nop    -> Nop
  Instruction (GetChar _) Nop  -> Nop
  Instruction fun Nop -> Instruction fun Nop

  Instruction fun next -> case removeFromEnd next of

    Nop   -> removeFromEnd $ Instruction fun Nop
    next' -> Instruction fun next'

  -- TODO: Remove loops
  Flow ctrl inner next -> Flow ctrl inner (removeFromEnd next)
