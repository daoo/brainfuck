{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Analysis where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Control.Applicative hiding (Const)
import Data.Maybe

-- |Check if an expression reads a certain memory position
exprDepends :: Int -> Expr -> Bool
exprDepends = ((.) isJust) . findVar

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
      _       -> (0, 0)

    expr :: Expr -> (Int, Int)
    expr = foldVarsL' (\x _ y -> x <+> (g y)) (0, 0)

    g :: Int -> (Int, Int)
    g d = case compare d 0 of
      LT -> (d, 0)
      EQ -> (0, 0)
      GT -> (0, d)

    (a, b) <+> (c, d) = (a + c, b + d)

-- |Check if some tarpit consists soley of PutChar (Const _) instructions
putConstOnly :: Tarpit -> Bool
putConstOnly = \case
  Nop                                  -> True
  Instruction (PutChar (Const _)) next -> putConstOnly next
  _                                    -> False

-- |Analyze a loop for a copy/multiply structure
-- A copy loop is a loop that follow these criteria:
--   * Contains no shifts, puts or gets.
--   * The loop memory position is decremented by 1. If it's decremented by some
--     other value we can not determine if it reaches zero or overflows.
--   * Increment or decrement any other memory cell by any integer.
-- If the supplied instruction isn't a Loop, we will return Nothing.
copyLoop :: Int -> Tarpit -> Maybe [(Int, Int)]
copyLoop d1 = go False
  where
    go b = \case
      Nop -> if b then Just [] else Nothing

      Instruction (Assign d2 (Var 1 d3 (Const c))) next
        | d1 == d2 && d2 == d3 && c == (-1) -> go True next
        | d1 /= d2 && d2 == d3              -> ((c, d2):) <$> go b next
        | otherwise                         -> Nothing

      _ -> Nothing

-- |Check if a while loop executes more than once
whileOnce :: Expr -> Tarpit -> Bool
whileOnce (Var 1 d (Const 0)) code = go False code
    where
      go b = \case
        Nop -> b

        Instruction (Assign d' (Const c)) next ->
          let a = d == d' in go (a && (c == 0) || b && (not a)) next

        Instruction (Assign _ _) next -> go b next

        Flow (If (Var 1 d' (Const 0))) inner next | d == d' ->
          let b' = go b inner in go b' next

        Instruction (GetChar _) _ -> False
        Instruction (PutChar _) _ -> False
        Instruction (Shift _) _   -> False
        Flow _ _ _                -> False

whileOnce _ _ = False
