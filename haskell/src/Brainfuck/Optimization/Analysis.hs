{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Analysis where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Control.Applicative ((<$>), (<|>))
import Data.List
import Data.Maybe

-- |Check if an expression uses the value of a certain memory offset
exprDepends :: Int -> Expr -> Bool
exprDepends d = unfold (||) (||) f
  where
    f (Get d') = d == d'
    f _        = False

-- |Analyze a IL Loop for copies
-- A copy loop is a loop that follow these criteria:
--   * Contains no shifts, puts or gets.
--   * The loop memory position is decremented by 1. The loop memory position
--     must be decremented by 1, otherwise we do not know if it will reach zero.
--   * Increment or decrement any other memory cell by any integer.
-- If the supplied instruction isn't a Loop, we will return Nothing.
copyLoop :: Int -> AST -> Maybe [(Int, Int)]
copyLoop d xs = do
  instrs <- instructionsOnly xs
  sets <- mapM setsOnly instrs
  adds <- mapM constantAddOnly sets
  let (dec, copies) = partition (g d) adds
  _ <- listToMaybe dec
  mapM h copies
  where
    instructionsOnly = \case
      Nop                  -> Just []
      Instruction fun next -> (fun :) <$> instructionsOnly next
      Flow _ _ _           -> Nothing

    setsOnly = \case
      Set d' e -> Just (d', e)
      _        -> Nothing

    constantAddOnly = \case
      (d1, Get d2 `Add` Const c) -> Just (d1, d2, c)
      (d1, Const c `Add` Get d2) -> Just (d1, d2, c)
      _                          -> Nothing

    -- Filter the decrement operation
    g d1 (d2, d3, -1) = d1 == d2 && d1 == d3
    g _ _             = False

    h (d1, d2, c) | d1 == d2  = Just (d1, c)
                  | otherwise = Nothing

memorySize :: AST -> (Int, Int)
memorySize = \case
  Nop                  -> (0, 0)
  Instruction fun next -> function fun <+> memorySize next
  Flow ctrl inner next -> control ctrl <+> memorySize inner <+> memorySize next

  where
    function = \case
      Set d e   -> g d <+> unfold (<+>) (<+>) expr e
      Shift d   -> g d
      PutChar e -> expr e
      GetChar d -> g d

    control = \case
      If e    -> expr e
      While e -> expr e
      _       -> (0, 0)

    expr = \case
      Get d -> g d
      _     -> g 0

    g :: Int -> (Int, Int)
    g d = case compare d 0 of
      LT -> (d, 0)
      EQ -> (0, 0)
      GT -> (0, d)

    (a, b) <+> (c, d) = (a + c, b + d)

-- |Check if the list of ILs make use of the memory or the global pointer
usesMemory :: AST -> Bool
usesMemory = \case
  Nop                  -> False
  Instruction fun next -> f fun || usesMemory next
  Flow _ inner next    -> usesMemory inner || usesMemory next

  where
    f = \case
      PutChar e -> unfold (||) (||) g e
      Set _ _   -> True
      GetChar _ -> True
      Shift _   -> True

    g (Get _) = True
    g _       = False

-- |Check if a memory position is set to zero by some ILs
setToZero :: Int -> AST -> Bool
setToZero d1 ast = maybe False (== 0) (go Nothing ast)
  where
    go t = \case
      Nop                  -> t
      Instruction fun next -> go (f fun <|> t) next
      Flow _ _ _           -> Nothing

    f = \case
      Set d2 (Const i) | d1 == d2 -> Just i
      _                           -> Nothing
