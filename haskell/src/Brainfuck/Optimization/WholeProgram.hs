{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.WholeProgram where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import qualified Data.Set as S

-- |Inline initial zeroes
inlineZeros :: AST -> AST
inlineZeros = go S.empty
  where
    go :: S.Set Int -> AST -> AST
    go s = \case
      Instruction fun next -> case fun of

        Assign i e -> Instruction (Assign i (inl s e)) (go (S.insert i s) next)
        PutChar e  -> Instruction (PutChar (inl s e)) (go s next)
        GetChar d  -> Instruction fun (go (S.delete d s) next)
        Shift _    -> Instruction fun next

      ast -> ast

    inl :: S.Set Int -> Expr -> Expr
    inl s = modifyValues (\case
      Var i | S.member i s -> Return $ Var i
            | otherwise    -> Return $ Const 0
      e                    -> Return e)

-- |Remove instructions from the end that does not performe any side effects
removeFromEnd :: AST -> AST
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
