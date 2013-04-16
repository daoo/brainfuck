{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.WholeProgram where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import qualified Data.IntSet as S

-- |Inline initial zeroes
-- Assumes that the memory is all zeroes from the start and inlines those
-- values into the code.
inlineZeros :: Tarpit -> Tarpit
inlineZeros = go S.empty
  where
    go :: S.IntSet -> Tarpit -> Tarpit
    go s = \case
      Instruction fun next -> case fun of

        Assign i e -> Instruction (Assign i (remove s e)) (go (S.insert i s) next)
        PutChar e  -> Instruction (PutChar (remove s e)) (go s next)
        GetChar d  -> Instruction fun (go (S.delete d s) next)
        Shift _    -> Instruction fun next

      code -> code

    remove :: S.IntSet -> Expr -> Expr
    remove s = filterVars ((`S.member` s) . snd)

-- |Remove instructions from the end that does not performe any side effects
removeFromEnd :: Tarpit -> Tarpit
removeFromEnd = \case
  Nop                          -> Nop
  Instruction (Assign _ _) Nop -> Nop
  Instruction (Shift _) Nop    -> Nop
  Instruction (GetChar _) Nop  -> Nop
  Instruction fun Nop          -> Instruction fun Nop

  Instruction fun next -> case removeFromEnd next of

    Nop   -> removeFromEnd $ Instruction fun Nop
    next' -> Instruction fun next'

  -- TODO: Remove loops
  Flow ctrl inner next -> Flow ctrl inner (removeFromEnd next)
