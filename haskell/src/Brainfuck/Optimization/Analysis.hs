{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Analysis where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Control.Applicative hiding (Const)
import Data.Maybe

-- |Check if an expression reads a certain memory position
exprDepends :: Int -> Expr -> Bool
exprDepends = ((.) isJust) . findExpr

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
    expr = foldl (\x y -> x <+> (g $ mkVar $ snd y)) (0, 0) . evars

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
      PutChar (Expr _ []) -> False
      PutChar (Expr _ _)  -> True
      Assign _ _          -> True
      GetChar _           -> True
      Shift _             -> True

-- |Analyze a loop for a copy/multiply structure
-- A copy loop is a loop that follow these criteria:
--   * Contains no shifts, puts or gets.
--   * The loop memory position is decremented by 1. If it's decremented by some
--     other value we can not determine if it reaches zero or overflows.
--   * Increment or decrement any other memory cell by any integer.
-- If the supplied instruction isn't a Loop, we will return Nothing.
copyLoop :: Int -> Tarpit -> Maybe [(Mult, Var)]
copyLoop d1 = go False
  where
    go b = \case
      Nop -> if b then Just [] else Nothing

      Instruction (Assign d2 (Expr c [(1, Var d3)])) next
        | d2 == d3 && d1 == d2 && c == -1 -> go True next
        | d2 == d3 && d1 /= d2            -> ((Mult c, Var d2):) <$> go b next
        | otherwise                       -> Nothing

      _ -> Nothing

-- |Check if a while loop executes more than once
whileOnce :: Expr -> Tarpit -> Bool
whileOnce (Expr 0 [(1, Var d)]) code = go d False code
  where
    go d1 b = \case
      Nop -> b

      Instruction (Assign d2 (Expr c [])) next ->
        let a = d1 == d2 in go d1 (a && (c == 0) || b && (not a)) next

      Instruction (Assign _ _) next -> go d1 b next

      Flow (If (Expr 0 [(1, Var d')])) inner next | d == d' ->
        let b' = go d1 b inner in go d1 b' next

      Instruction (GetChar _) _ -> False
      Instruction (PutChar _) _ -> False
      Instruction (Shift _) _   -> False
      Flow _ _ _                -> False

whileOnce _ _ = False
