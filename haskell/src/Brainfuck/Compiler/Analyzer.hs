module Brainfuck.Compiler.Analyzer where

import Data.List

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL

-- |Count the number of instructions
-- Descends into loops
ilCount :: [IL] -> Int
ilCount = sum . map f
  where
    f (Loop _ ils) = 1 + ilCount ils
    f _            = 1

-- |Calculate the depth of the loop tree
loopDepth :: IL -> Int
loopDepth (Loop _ ils) = (+1) $ maximum $ map loopDepth ils
loopDepth _            = 0

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
copyLoop (Loop o loop) = helper
  where
    helper | null a || not (null d) = Nothing
           | otherwise              = Just $ map getConst c
      where
        (a, b) = partition (isDec o) loop
        (c, d) = partition isConstAdd b

    getConst (Set d (Get _ `Add` Const v)) = (d, v)
    getConst (Set d (Const v `Add` Get _)) = (d, v)
    getConst _ = error "Not a const add"

    isConstAdd (Set d1 (Get d2 `Add` Const _)) = d1 == d2
    isConstAdd (Set d1 (Const _ `Add` Get d2)) = d1 == d2
    isConstAdd _                               = False

    isDec d1 (Set d2 (Get d3 `Add` Const (-1))) = d1 == d2 && d1 == d3
    isDec d1 (Set d2 (Const (-1) `Add` Get d3)) = d1 == d2 && d1 == d3
    isDec _ _                                   = False

copyLoop _ = Nothing
