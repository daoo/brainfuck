{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compiler.Analysis where

import Brainfuck.Data.Expr
import Brainfuck.Data.IL
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
copyLoop :: Int -> [IL] -> Maybe [(Int, Int)]
copyLoop d xs = do
  sets <- mapM f xs
  let (dec, copies) = partition (g d) sets
  listToMaybe dec
  mapM h copies
  where
    -- First filter, if a non-constant add is found, exit
    f = \case
      Set d1 (Get d2 `Add` Const c) -> Just (d1, d2, c)
      Set d1 (Const c `Add` Get d2) -> Just (d1, d2, c)
      _                             -> Nothing

    -- Filter the decrement operation
    g d1 (d2, d3, -1) = d1 == d2 && d1 == d3
    g _ _             = False

    h (d1, d2, c) | d1 == d2  = Just (d1, c)
                  | otherwise = Nothing

memoryAccess :: [IL] -> [[Int]]
memoryAccess = go 0
  where
    go _ []     = []
    go i (x:xs) = case x of
      If _ _    -> error "FIXME: memoryAccess If"
      While _ _ -> error "FIXME: memoryAccess While"
      Shift s   -> go (i + s) xs
      Set d e   -> ((i + d) : expr i e) : go i xs
      PutChar e -> expr i e             : go i xs
      GetChar d -> [i + d]              : go i xs

    expr i = unfold (++) (++) f
      where
        f (Get d) = [i + d]
        f _       = []

-- |Approximate how much memory is needed
memorySize :: [IL] -> (Int, Int)
memorySize xs = case concat $ memoryAccess xs of
  []  -> (0, 0)
  xs' -> (minimum xs', maximum xs')

-- |Check if the list of ILs make use of the memory or the global pointer
usesMemory :: [IL] -> Bool
usesMemory = any $ \case
  If _ ys    -> usesMemory ys
  While _ ys -> usesMemory ys
  PutChar e  -> unfold (||) (||) g e
  Set _ _    -> True
  GetChar _  -> True
  Shift _    -> True
  where
    g (Get _) = True
    g _       = False

-- |Check if a memory position is set to zero by some ILs
setToZero :: Int -> [IL] -> Maybe [IL]
setToZero d1 = fmap reverse . go . reverse
  where
    go []       = Just []
    go (x : xs) = case x of
      If _ _                      -> Nothing
      While _ _                   -> Nothing
      Shift _                     -> Nothing
      GetChar d2       | d1 == d2 -> Nothing
      Set d2 (Const 0) | d1 == d2 -> Just xs
      Set d2 _         | d1 == d2 -> Nothing
      _                           -> fmap (x :) $ go xs
