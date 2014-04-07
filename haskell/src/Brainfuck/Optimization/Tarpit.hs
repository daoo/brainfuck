{-# LANGUAGE LambdaCase, BangPatterns #-}
module Brainfuck.Optimization.Tarpit where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Data.Monoid

type Reduction = Tarpit -> Maybe Tarpit

reduce :: Reduction -> Tarpit -> Tarpit
reduce f a = case a of
  Nop -> Nop

  Instruction fun next -> let a' = Instruction fun (reduce f next) in
    case f a' of
      Nothing  -> a'
      Just a'' -> reduce f a''

  Flow ctrl inner next -> let a' = Flow ctrl (reduce f inner) (reduce f next) in
    case f a' of
      Nothing  -> a'
      Just a'' -> reduce f a''

-- |Combine two reductions.
--
-- Tries both, or either, or none.
pipe :: Reduction -> Reduction -> Reduction
pipe f g a = case f a of
  Nothing -> g a
  Just a' -> case g a' of
    Nothing  -> Just a'
    Just a'' -> Just a''

-- |Reduce flow control statements.
--
-- Reduces never-running and infinite loops.
flowReduction :: Tarpit -> Maybe Tarpit
flowReduction = \case
  Flow (While (Const 0)) _     next -> return $ next
  Flow (If    (Const 0)) _     next -> return $ next
  Flow (While (Const _)) inner _    -> return $ Flow (While (Const 1)) inner Nop
  Flow (If    (Const _)) inner next -> return $ inner `mappend` next

  _ -> Nothing

-- |Reduce while loops to if statements (see 'whileOnce').
whileReduction :: Tarpit -> Maybe Tarpit
whileReduction = \case
  Flow (While e@(Var 1 d (Const 0))) inner next ->
    fmap (f e d next) (whileOnce d inner)

  _ -> Nothing

  where
    f e d next inner' = Flow (If e) inner' (Instruction (Assign d $ Const 0) next)

-- |Reduce copy loops (see 'copyLoop').
copyLoopReduction :: Tarpit -> Maybe Tarpit
copyLoopReduction = \case
  Flow (While (Var 1 d (Const 0))) inner next ->
    fmap (\inner' -> inner' <> next) (copyLoop d inner)

  _ -> Nothing

-- |Move put instructions to after assignments by inlining the assignment.
--
-- Allows expressions in put instructions to be fully evaluated.
putReduction :: Tarpit -> Maybe Tarpit
putReduction = \case
  Instruction fun@(Assign d e1)
    (Instruction (PutChar e2) next) -> Just $
      Instruction
        (PutChar $ insertExpr d e1 e2)
        (Instruction fun next)

  _ -> Nothing
