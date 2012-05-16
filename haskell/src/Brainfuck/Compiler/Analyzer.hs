module Brainfuck.Compiler.Analyzer where

import Data.List

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Ext

-- |Count the number of instructions
-- Descends into loops
ilCount :: [IL] -> Int
ilCount = sum . map f
  where
    f (While _ ils) = 1 + ilCount ils
    f _             = 1

-- |Calculate the depth of the loop tree
loopDepth :: IL -> Int
loopDepth (While _ ils) = (+1) $ maximum $ map loopDepth ils
loopDepth _             = 0

-- |Check if an expression uses the value of a certain memory offset
exprDepends :: Int -> Expr -> Bool
exprDepends i (Get d)     = i == d
exprDepends i (Add e1 e2) = exprDepends i e1 || exprDepends i e2
exprDepends i (Mul e1 e2) = exprDepends i e1 || exprDepends i e2
exprDepends _ _           = False

-- |Analyze a IL Loop for copies
-- A copy loop is a loop that follow these criteria:
--   * Contains no shifts, puts or gets
--   * The loop memory position is decremented by 1
--   * Increment or decrement any other memory cell by any integer
-- If the supplied instruction isn't a Loop, we will return Nothing.
copyLoop :: IL -> Maybe [(Int, Int)]
copyLoop (While d xs) = do
  sets <- mapM f xs
  let (dec, copies) = partition (g d) sets
  onlyOne dec
  mapM h copies
  where
    -- First filter, if a non-constant add is found, exit
    f (Set d1 (Get d2 `Add` Const c)) = Just (d1, d2, c)
    f (Set d1 (Const c `Add` Get d2)) = Just (d1, d2, c)
    f _                                          = Nothing

    -- Filter the decrement operation
    g d1 (d2, d3, (-1)) = d1 == d2 && d1 == d3
    g _ _               = False

    h (d1, d2, c) | d1 == d2  = Just (d1, c)
                  | otherwise = Nothing

copyLoop _ = Nothing

-- |Returns True if the list of ILs has any shifts.
hasShifts :: [IL] -> Bool
hasShifts = any f
  where
    f (While _ loop) = hasShifts loop
    f (Shift _)      = True
    f _              = False

-- |Check if the list of ILs make use of the memory or the global pointer
usesMemory :: [IL] -> Bool
usesMemory = any f
  where
    f (While _ loop) = usesMemory loop
    f (Set _ e)      = g e
    f (PutChar e)    = g e
    f (GetChar _)    = False
    f (Shift _)      = False

    g (Const _)   = False
    g (Get _)     = True
    g (Add e1 e2) = g e1 || g e2
    g (Mul e1 e2) = g e1 || g e2

data Occurs = Nope | Once | SetTo | InLoop

shouldInline :: Occurs -> Expr -> Bool
shouldInline SetTo e  = complexity e <= 4
shouldInline Once e   = complexity e <= 3
shouldInline Nope e   = complexity e <= 1
shouldInline InLoop _ = False

occurrs :: Int -> [IL] -> Occurs
occurrs _ []                 = Nope
occurrs d (x : xs) = case x of
  While d' ys | d == d'   -> InLoop
              | otherwise -> case occurrs d ys of
    Nope -> occurrs d xs
    _    -> InLoop

  Set d' e   | d == d'           -> SetTo
             | d `exprDepends` e -> Once
  PutChar e  | d `exprDepends` e -> Once
  GetChar d' | d == d'           -> SetTo

  _ -> occurrs d xs
