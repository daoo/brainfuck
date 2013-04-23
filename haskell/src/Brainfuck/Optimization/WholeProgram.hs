{-# LANGUAGE LambdaCase, BangPatterns #-}
module Brainfuck.Optimization.WholeProgram
  ( inlineZeros
  , removeFromEnd
  , unrollEntierly
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Data.Monoid
import qualified Data.IntMap as M
import qualified Data.IntSet as S

-- |Inline initial zeroes
-- Assumes that the memory is all zeroes from the start and inlines those
-- values into the code.
inlineZeros :: Tarpit -> Tarpit
inlineZeros = go S.empty
  where
    go :: S.IntSet -> Tarpit -> Tarpit
    go s = \case
      Nop -> Nop

      Instruction fun next -> case fun of

        Assign i e -> Instruction (Assign i (remove s e)) (go (S.insert i s) next)
        PutChar e  -> Instruction (PutChar (remove s e)) (go s next)
        GetChar d  -> Instruction fun (go (S.delete d s) next)
        Shift _    -> Instruction fun next

      Flow (If e) inner next -> case remove s e of

        Const 0 -> go s next
        Const _ -> go s $ inner `mappend` next
        e'      -> Flow (If e') (go s inner) next

      Flow (While e) inner next -> case remove s e of

        Const 0 -> go s next
        Const _ -> Flow (While $ Const 1) inner Nop
        _       -> Flow (While e) inner next

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

-- |Loop unrolling optimization
-- Fully unrolls the entire program, only works for terminating programs that
-- don't depend on inupt.
unrollEntierly :: Tarpit -> Tarpit
unrollEntierly = go M.empty
  where
    go !m = \case
      Nop -> Nop

      Instruction fun next -> case fun of

        Assign d e -> case expr m e of

          Const c -> go (M.insert d c m) next
          _       -> Instruction fun next

        PutChar e -> Instruction (PutChar $ expr m e) (go m next)
        Shift s   -> go (shift s m) next

        GetChar _ -> Instruction fun next

      Flow (If e) inner next -> case expr m e of

        Const 0 -> go m next
        Const _ -> go m $ inner `mappend` next
        e'      -> Flow (If e') (go m inner) next

      Flow (While e) inner next -> case expr m e of

        Const 0 -> go m next
        _       -> go m $ inner `mappend` Flow (While e) inner next

    expr, constants, zeros :: M.IntMap Int -> Expr -> Expr
    expr m    = constants m . zeros m
    constants = flip $ M.foldrWithKey' insertConstant
    zeros m   = filterVars ((`M.member` m) . snd)
    shift s m = M.mapKeysMonotonic (subtract s) m
