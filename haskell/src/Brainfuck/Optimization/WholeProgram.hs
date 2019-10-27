{-# LANGUAGE LambdaCase, BangPatterns #-}
module Brainfuck.Optimization.WholeProgram
  ( inlineShifts
  , inlineConstants
  , inlineZeros
  , removeFromEnd
  , unrollEntierly
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import qualified Data.IntMap as M
import qualified Data.IntSet as S

-- |Reduce most shift instructions by inlining them into assignments and
-- expressions.
--
-- Whole program pass that keeps track of the current shift amount and adds it
-- to assignments and expressions. Some shifts can't be inlined, for instance
-- shifts in loops as we don't know how many times a loop will execute we don't
-- know how many shifts there will be. We still accumulate all shifts within a
-- loop into one instruction.
inlineShifts :: Tarpit -> Tarpit
inlineShifts = go 0 0
  where
    go !s !t = \case
      Nop | t == 0    -> Nop
          | otherwise -> Instruction (Shift t) Nop

      Instruction fun next -> case fun of

        GetChar d  -> Instruction (GetChar $ d + s)                $ go s t next
        PutChar e  -> Instruction (PutChar $ shiftExpr e s)        $ go s t next
        Assign d e -> Instruction (Assign (d + s) $ shiftExpr e s) $ go s t next
        Shift s'   -> go (s + s') (t + s') next

      Flow ctrl inner next -> case ctrl of
        If e    -> Flow (If $ shiftExpr e s)    (go s 0 inner) (go s t next)
        While e -> Flow (While $ shiftExpr e s) (go s 0 inner) (go s t next)

-- |Reduce expressions by inlining known constants in the begining.
inlineConstants :: Tarpit -> Tarpit
inlineConstants = go M.empty
  where
    go :: M.IntMap Int -> Tarpit -> Tarpit
    go m = \case
      Nop -> Nop

      Instruction fun next -> case fun of

        GetChar d             -> Instruction fun                  $ go (M.delete d m) next
        PutChar e             -> Instruction (PutChar $ expr e m) $ go m next
        Assign d (Constant c) -> Instruction fun                  $ go (M.insert d c m) next
        Shift s               -> Instruction fun                  $ go (shift s m) next

        Assign d e -> case expr e m of

          e'@(Constant c) -> Instruction (Assign d e') $ go (M.insert d c m) next
          e'              -> Instruction (Assign d e') $ go (M.delete d m)   next

      Flow ctrl inner next -> case ctrl of

        If e -> case expr e m of

          Constant 0 -> go m next
          Constant _ -> go m $ inner <> next
          e'         -> Flow (If e') (go m inner) (go M.empty next)

        While e -> case expr e m of

          Constant 0 -> go m next
          _          -> Flow (While e) (go M.empty inner) (go M.empty next)

    expr  = M.foldrWithKey' ((flip insertConst .) . (,))
    shift = M.mapKeysMonotonic . subtract

-- |Inline initial zeroes.
--
-- Assumes that the memory is all zeroes from the start and inlines those
-- values into the code.
inlineZeros :: Tarpit -> Tarpit
inlineZeros = go S.empty
  where
    go :: S.IntSet -> Tarpit -> Tarpit
    go !s = \case
      Nop -> Nop

      Instruction fun next -> case fun of

        Assign i e -> Instruction (Assign i $ remove s e) $ go (S.insert i s) next
        PutChar e  -> Instruction (PutChar $ remove s e)  $ go s next
        GetChar d  -> Instruction fun                     $ go (S.delete d s) next
        Shift s'   -> Instruction fun                     $ go (shift s' s) next

      Flow (If e) inner next -> case remove s e of

        Constant 0 -> go s next
        Constant _ -> go s $ inner <> next
        e'         -> Flow (If e') (go s inner) next

      Flow (While e) inner next -> case remove s e of

        Constant 0 -> go s next
        Constant _ -> Flow (While (Constant 1)) inner Nop
        _          -> Flow (While e) inner next

    remove s = filterVars (`S.member` s)
    shift    = S.map . subtract

-- |Remove instructions from the end that does not perform any side effects.
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

-- |Loop unrolling optimization.
--
-- Fully unrolls the entire program up to a unroll limit. Ignores GetChar and
-- thus only works proprely for programs not depending on input.
unrollEntierly :: Tarpit -> Tarpit
unrollEntierly = go 1000000 M.empty
  where
    go  0  _ e = e
    go !n !m e = case e of
      Nop -> Nop

      Instruction fun next -> case fun of

        Assign d e -> go n (M.insert d (eval m e) m) next
        Shift s    -> go n (shift s m) next
        PutChar e  -> Instruction (PutChar $ Constant (eval m e)) (go n m next)
        GetChar _  -> Instruction fun next

      Flow (If e) inner next -> go n m $ if eval m e == 0
        then next
        else inner <> next

      Flow (While e) inner next -> go (n - 1) m $ if eval m e == 0
        then next
        else inner <> Flow (While e) inner next

    shift = M.mapKeysMonotonic . subtract

    eval m = evalExpr (\k -> M.findWithDefault 0 k m)
