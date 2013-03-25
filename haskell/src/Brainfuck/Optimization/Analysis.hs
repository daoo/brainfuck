{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Analysis where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Control.Applicative hiding (Const)
import Data.List
import Data.Maybe

-- |Check if an expression reads a certain memory position
exprDepends :: Int -> Expr -> Bool
exprDepends d = unfold (||) (flip const) (const False) (== d)

-- |Analyze a IL Loop for copies
-- A copy loop is a loop that follow these criteria:
--   * Contains no shifts, puts or gets.
--   * The loop memory position is decremented by 1. The loop memory position
--     must be decremented by 1, otherwise we do not know if it will reach zero.
--   * Increment or decrement any other memory cell by any integer.
-- If the supplied instruction isn't a Loop, we will return Nothing.
copyLoop :: Int -> AST -> Maybe [(Int, Int)]
copyLoop d xs = do
  funs <- funsOnly xs
  sets <- mapM setsOnly funs
  adds <- mapM constantAddOnly sets
  let (dec, copies) = partition (g d) adds
  _ <- listToMaybe dec
  mapM h copies
  where
    funsOnly = \case
      Nop                  -> Just []
      Instruction fun next -> (fun :) <$> funsOnly next
      Flow _ _ _           -> Nothing

    setsOnly = \case
      Assign d' e -> Just (d', e)
      _           -> Nothing

    constantAddOnly = \case
      (d1, Add (Var d2) (Const c)) -> Just (d1, d2, c)
      (d1, Add (Const c) (Var d2)) -> Just (d1, d2, c)
      _                            -> Nothing

    -- Filter the decrement operation
    g d1 (d2, d3, i) = d1 == d2 && d1 == d3 && i == -1

    h (d1, d2, c) | d1 == d2  = Just (d1, c)
                  | otherwise = Nothing

-- |Heuristically decide how much memory a program uses.
memorySize :: AST -> (Int, Int)
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
usesMemory :: AST -> Bool
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
whileOnce :: Expr -> AST -> Bool
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
