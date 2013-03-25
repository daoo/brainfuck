{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Analysis where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Control.Applicative hiding (Const)

-- |Check if an expression reads a certain memory position
exprDepends :: Int -> Expr -> Bool
exprDepends d = unfold (||) (flip const) (const False) (== d)

-- |Analyze a loop for a copy/multiply structure
-- A copy loop is a loop that follow these criteria:
--   * Contains no shifts, puts or gets.
--   * The loop memory position is decremented by 1. If it's decremented by some
--     other value we can not determine if it reaches zero or overflows.
--   * Increment or decrement any other memory cell by any integer.
-- If the supplied instruction isn't a Loop, we will return Nothing.
copyLoop :: Int -> Tarpit -> Maybe [(Int, Int)]
copyLoop d1 = go
  where
    go = \case
      Nop -> Just []

      Instruction (Assign d2 (Var d3 `Add` Const c)) next -> f d2 d3 c next
      Instruction (Assign d2 (Const c `Add` Var d3)) next -> f d2 d3 c next

      _ -> Nothing

    f d2 d3 c next
      | d2 == d3 && d1 == d2 && c == -1 = go next
      | d2 == d3 && d1 /= d2            = ((d2, c):) <$> go next
      | otherwise                       = Nothing

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

    expr = unfold (<+>) (flip const) (const (0, 0)) g

    g :: Int -> (Int, Int)
    g d = case compare d 0 of
      LT -> (d, 0)
      EQ -> (0, 0)
      GT -> (0, d)

    (a, b) <+> (c, d) = (a + c, b + d)

-- |Check if some program uses the memory or the global pointer
usesMemory :: Tarpit -> Bool
usesMemory = \case
  Nop                  -> False
  Instruction fun next -> f fun || usesMemory next
  Flow _ inner next    -> usesMemory inner || usesMemory next

  where
    f = \case
      PutChar e  -> unfold (||) (flip const) (const False) (const True) e
      Assign _ _ -> True
      GetChar _  -> True
      Shift _    -> True

-- |Check if a while loop executes more than once
whileOnce :: Expr -> Tarpit -> Bool
whileOnce e ast = case e of
  Var d -> go d False ast
  _     -> False

  where
    go d1 b = \case
      Nop -> b

      Instruction (Assign d2 (Const i)) next ->
        let a = d1 == d2 in go d1 (a && (i == 0) || b && (not a)) next

      Instruction (Assign _ _) next -> go d1 b next

      Flow (If e') inner next | e == e' ->
        let b' = go d1 b inner in go d1 b' next

      Instruction (GetChar _) _ -> False
      Instruction (PutChar _) _ -> False
      Instruction (Shift _) _   -> False
      Flow _ _ _                -> False
