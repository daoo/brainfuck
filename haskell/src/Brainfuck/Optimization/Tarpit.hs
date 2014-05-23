{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Tarpit where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Applicative hiding (Const)
import Data.Monoid

type Reduction = Tarpit -> Maybe Tarpit

reduce :: Reduction -> Tarpit -> Tarpit
reduce f = \case
  Nop                  -> Nop
  Instruction fun next -> helper (Instruction fun (reduce f next))
  Flow ctrl inner next -> helper (Flow ctrl (reduce f inner) (reduce f next))

  where
    helper a = case f a of
      Nothing -> a
      Just a' -> reduce f a'

-- |Combine two reductions.
--
-- Tries both, either, or none. The following properties holds:
--
-- prop> pure # pure == id
-- prop> const empty # const empty == id
(#) :: Reduction -> Reduction -> Reduction
(#) f g a = (f a >>= g) <|> f a <|> g a

-- |Reduce flow control statements.
--
-- Reduces never-running and infinite loops.
flowReduction :: Tarpit -> Maybe Tarpit
flowReduction = \case
  Flow (While e) inner next
    | isZero  e -> return next
    | isConst e -> return (Flow (While (econst 1)) inner Nop)

  Flow (If e) inner next
    | isZero  e -> return next
    | isConst e -> return (inner <> next)

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
