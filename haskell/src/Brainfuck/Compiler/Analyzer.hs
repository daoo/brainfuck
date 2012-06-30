module Brainfuck.Compiler.Analyzer where

import Data.List

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Ext

-- |Check if an expression uses the value of a certain memory offset
exprDepends :: Int -> Expr -> Bool
exprDepends d = unfold (||) (||) f
  where
    f (Get d') = d == d'
    f _        = False

-- |Analyze a IL Loop for copies
-- A copy loop is a loop that follow these criteria:
--   * Contains no shifts, puts or gets
--   * The loop memory position is decremented by 1
--   * Increment or decrement any other memory cell by any integer
-- If the supplied instruction isn't a Loop, we will return Nothing.
copyLoop :: Int -> [IL] -> Maybe [(Int, Int)]
copyLoop d xs = do
  sets <- mapM f xs
  let (dec, copies) = partition (g d) sets
  onlyOne dec
  mapM h copies
  where
    -- First filter, if a non-constant add is found, exit
    f (Set d1 (Get d2 `Add` Const c)) = Just (d1, d2, c)
    f (Set d1 (Const c `Add` Get d2)) = Just (d1, d2, c)
    f _                               = Nothing

    -- Filter the decrement operation
    g d1 (d2, d3, -1) = d1 == d2 && d1 == d3
    g _ _             = False

    h (d1, d2, c) | d1 == d2  = Just (d1, c)
                  | otherwise = Nothing

memoryAccess :: [IL] -> [[Int]]
memoryAccess = go 0
  where
    go _ []               = []
    go _ (While _ _ : _)  = error "FIXME: While"
    go _ (If _ _    : _)  = error "FIXME: If"
    go i (Shift s   : xs) = go (i + s) xs
    go i (Set d e   : xs) = ((i + d) : expr i e) : go i xs
    go i (PutChar e : xs) = expr i e             : go i xs
    go i (GetChar d : xs) = [i + d]              : go i xs

    expr i = map (+i) . unfold (++) (++) f
      where
        f (Get d) = [d]
        f _       = []

-- |Analyze how much memory is needed
memorySize :: [IL] -> (Int, Int)
memorySize xs = case concat $ memoryAccess xs of
  []  -> (0, 0)
  xs' -> (minimum xs', maximum xs')

-- |Check if the list of ILs make use of the memory or the global pointer
usesMemory :: [IL] -> Bool
usesMemory = any f
  where
    f (While _ ys) = usesMemory ys
    f (If _ ys)    = usesMemory ys
    f (Set _ _)    = True
    f (PutChar e)  = unfold (||) (||) g e
    f (GetChar _)  = True
    f (Shift _)    = True

    g (Get _) = True
    g _       = False

setToZero :: Int -> [IL] -> Maybe [IL]
setToZero d1 = fmap reverse . go . reverse
  where
    go []       = Just []
    go (x : xs) = case x of
      While _ _                   -> Nothing
      If _ _                      -> Nothing
      Shift _                     -> Nothing
      GetChar d2       | d1 == d2 -> Nothing
      Set d2 (Const 0) | d1 == d2 -> Just xs
      Set d2 _         | d1 == d2 -> Nothing
      _                           -> fmap (x :) $ go xs
