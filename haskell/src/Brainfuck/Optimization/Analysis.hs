{-# LANGUAGE LambdaCase, BangPatterns #-}
module Brainfuck.Optimization.Analysis
  ( exprDepends
  , memorySize
  , putConstOnly
  , copyLoop
  , whileOnce
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Data.Maybe

-- |Check if an expression reads a certain variable
exprDepends :: Int -> Expr -> Bool
exprDepends = (isJust .) . findVar

-- |Heuristically decide how much memory a program uses.
memorySize :: Tarpit -> (Int, Int)
memorySize = \case
  Nop                  -> (0, 0)
  Instruction fun next -> function fun <+> memorySize next
  Flow ctrl inner next -> control ctrl <+> memorySize inner <+> memorySize next

  where
    function = \case
      Assign d e -> g d <+> expr e
      Shift d    -> g d
      PutChar e  -> expr e
      GetChar d  -> g d

    control = \case
      If e    -> expr e
      While e -> expr e

    expr = foldVarsL' (\x _ y -> x <+> g y) (0, 0)

    g d = case compare d 0 of
      LT -> (d, 0)
      EQ -> (0, 0)
      GT -> (0, d)

    (a, b) <+> (c, d) = (a + c, b + d)

-- |Check if some tarpit consists soley of PutChar (Const _) instructions
putConstOnly :: Tarpit -> Bool
putConstOnly = \case
  Nop                          -> True
  Instruction (PutChar e) next -> isConst e && putConstOnly next
  _                            -> False

-- |Analyze a loop for a copy/multiply structure
--
-- A copy loop is a loop that follow the following criteria:
--
--   * Contains no shifts, puts or gets.
--   * The loop memory position is decremented by 1. If it's decremented by some
--     other value we can not determine if it reaches zero or overflows.
--   * Increments or decrements any other memory cell by any integer.
copyLoop :: Int -> Tarpit -> Maybe Tarpit
copyLoop d1 = go
  where
    go = \case
      Nop -> Just $ Instruction (d1 &= 0) Nop

      Instruction (Assign d2 (Var 1 d3 (Const c))) next
        | d1 == d2 && d2 == d3 && c == (-1) -> go next
        | d1 /= d2 && d2 == d3              -> Instruction (Assign d2 $ mult d2 c) <$> go next
        | otherwise                         -> Nothing

      _ -> Nothing

    mult d2 c = (c .* evar d1) .+ evar d2

-- |Check if a while loop could be an if statement.
--
-- When the loop is conditioned over a variable and that variable is assigned
-- the value zero in the loop body we know that the loop will only be executed
-- once. Thus we can convert it to an if statement and the zero assignment can
-- be moved out to after the if statement (as long as no values in the loop
-- body relies on the value of the loop condition).
whileOnce :: Int -> Tarpit -> Maybe Tarpit
whileOnce d xs = if find False xs
  then Just $ filt xs
  else Nothing
  where
    find !b = \case
      Nop -> b

      Instruction fun next -> case fun of
        Assign d' (Const 0) | d == d' -> find True next
        Assign d' _         | d == d' -> find False next
        GetChar d'          | d == d' -> find False next

        Shift _ -> False

        _ -> find b next

      Flow _ _ next -> find False next

    -- TODO: Only remove the last instruction
    filt = \case
      Nop -> Nop

      Instruction fun next -> case fun of

        Assign d' e | isZero e && d == d' -> filt next
        _                                 -> Instruction fun $ filt next

      Flow ctrl inner next -> Flow ctrl inner $ filt next
