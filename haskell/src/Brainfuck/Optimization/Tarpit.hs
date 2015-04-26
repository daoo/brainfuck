{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Tarpit
  ( flowReduction
  , whileReduction
  , copyLoopReduction
  , putReduction
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Data.Monoid

{-# INLINE reduce #-}
reduce :: (Tarpit -> Maybe Tarpit) -> Tarpit -> Tarpit
reduce f = \case
  Nop                  -> Nop
  Instruction fun next -> run (Instruction fun (reduce f next))
  Flow ctrl inner next -> run (Flow ctrl (reduce f inner) (reduce f next))

  where
    run a = maybe a (reduce f) (f a)

-- |Reduce flow control statements.
--
-- Reduces never-running and infinite loops.
flowReduction :: Tarpit -> Tarpit
flowReduction = reduce $ \case
  Flow (While (Constant 0)) _     next -> pure next
  Flow (While (Constant _)) inner _    -> pure (Flow (While (Constant 1)) inner Nop)

  Flow (If (Constant 0)) _     next -> pure next
  Flow (If (Constant _)) inner next -> pure (inner <> next)

  _ -> Nothing

-- |Reduce while loops to if statements (see 'whileOnce').
whileReduction :: Tarpit -> Tarpit
whileReduction = reduce $ \case
  Flow (While e@(Variable1 d)) inner next ->
    fmap (f e d next) (whileOnce d inner)

  _ -> Nothing

  where
    f e d next inner' = Flow (If e) inner' (Instruction (Assign d $ Constant 0) next)

-- |Reduce copy loops (see 'copyLoop').
copyLoopReduction :: Tarpit -> Tarpit
copyLoopReduction = reduce $ \case
  Flow (While (Variable1 d)) inner next ->
    fmap (<> next) (copyLoop d inner)

  _ -> Nothing

-- |Move put instructions to after assignments by inlining the assignment.
--
-- Allows expressions in put instructions to be fully evaluated.
putReduction :: Tarpit -> Tarpit
putReduction = reduce $ \case
  Instruction fun@(Assign d e1)
    (Instruction (PutChar e2) next) -> pure $
      Instruction
        (PutChar $ insertExpr e2 (d, e1))
        (Instruction fun next)

  _ -> Nothing
