module Properties where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Inlining
import Brainfuck.Compiler.Optimize
import Brainfuck.Data.Brainfuck
import Brainfuck.Data.Expr
import Brainfuck.Data.IL
import Brainfuck.Data.State
import Brainfuck.Ext
import Brainfuck.Interpreter
import Brainfuck.Parser
import Data.ListZipper
import Data.Sequence (empty)
import Data.Word
import Test.QuickCheck

-- {{{ ListZipper
propZipperMoveSize :: Int -> ListZipper a -> Bool
propZipperMoveSize i a = size a == size (move i a)
-- }}}
-- {{{ Misc
propMapIndexEq :: Int -> NonEmptyList Int -> Property
propMapIndexEq x (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \i -> (mapIndex (const x) i xs !! i) == x

propMapIndexLength :: Int -> NonEmptyList Int -> Property
propMapIndexLength x (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \i -> length (mapIndex (const x) i xs) == length xs

propPipeAdd :: Property
propPipeAdd = forAll (choose (0, 20000)) $
  \i -> pipe (replicate i (+1)) 0 == i

propWhileModified :: Property
propWhileModified = forAll (choose (-10000000, 10000000)) ((== 0) . whileModified f)
  where
    f :: Int -> Int
    f j | j < 0     = j + 1
        | j == 0    = 0
        | otherwise = j - 1
-- }}}
-- {{{ Parser
propParser :: [Brainfuck] -> Bool
propParser bf = case parseBrainfuck (show bf) of
  Left _    -> False
  Right bf' -> bf == bf'
-- }}}
-- {{{ Optimization
compareFull :: (Integral a) => Int -> State a -> State a -> Bool
compareFull i (State _ out1 m1) (State _ out2 m2) = cut i m1 == cut i m2 && out1 == out2

compareOutput :: [IL] -> [IL] -> Bool
compareOutput xs ys = getOutput (run state xs) == getOutput (run state ys)
  where
    state :: State Word8
    state = State [1..] empty newMemory

testCode :: [IL] -> [IL] -> Bool
testCode xs ys = compareFull s (run state xs) (run state ys)
  where
    s = let (xsMin, xsMax) = memorySize xs
            (ysMin, ysMax) = memorySize ys
         in 1 + abs xsMin + abs xsMax + abs ysMin + abs ysMax

    state :: State Word8
    state = State [1..] empty newMemory

propOptimize :: ([IL] -> [IL]) -> [IL] -> Bool
propOptimize f xs = testCode xs (f xs)

-- TODO: Better testing

propInline :: Int -> Expr -> [IL] -> Bool
propInline d e xs = inline d e xs `testCode` (Set d e : xs)

propHeuristicInlining :: Int -> Expr -> [IL] -> Bool
propHeuristicInlining d e xs = case heuristicInlining d e xs of
  Just xs' -> (Set d e : xs) `testCode` xs'
  Nothing  -> True

propOptimizeInlineZeros, propOptimizeCopies, propOptimizeCleanUp,
  propOptimizeExpressions, propOptimizeInlining :: [IL] -> Bool

propOptimizeCleanUp     = propOptimize cleanUp
propOptimizeCopies      = propOptimize reduceCopyLoops
propOptimizeExpressions = propOptimize $ mapIL optimizeExpressions
propOptimizeInlineZeros = propOptimize inlineZeros
propOptimizeInlining    = propOptimize inlining

propOptimizeMoveShifts :: [IL] -> Bool
propOptimizeMoveShifts xs = memoryAccess xs == memoryAccess (moveShifts xs)

propOptimizeAll :: [IL] -> Bool
propOptimizeAll xs = compareOutput xs (optimizeAll xs)

-- }}}
-- {{{ Loops
exCopyLoop1 :: IL
exCopyLoop1 =
  While (Get 0)
    [ Set 0 $ Get 0 `Add` Const (-1) ]

exCopyLoop2 :: IL
exCopyLoop2 =
  While (Get 5)
    [ Set 5 $ Get 5 `Add` Const (-1)
    , Set 1 $ Get 1 `Add` Const 1
    , Set 2 $ Get 2 `Add` Const 5
    , Set 3 $ Get 3 `Add` Const 10 ]

exNotCopyLoop1 :: IL
exNotCopyLoop1 =
  While (Get 5)
    [ Set 5 $ Get 5 `Add` Const (-1)
    , Set 6 $ Get 5 `Add` Const 10 ]

exShiftLoop1 :: [IL]
exShiftLoop1 =
  [ Set 0 $ Get 0 `Add` Const 10
  , Set 1 $ Const 0
  , Set 2 $ Get 2 `Add` Const 4 `Add` Get 0
  , Set 3 $ Get 3 `Add` Const 5
  , While (Get 3) [ Shift (-1) ] ]

-- }}}
-- {{{ Expressions
propExprOptimizeTwice :: Expr -> Bool
propExprOptimizeTwice e = let e' = optimizeExpr e in e' == optimizeExpr e'

propExprEval :: Expr -> NonEmptyList Int -> Bool
propExprEval e (NonEmpty xs) = eval f e == eval f (optimizeExpr e)
  where
    f = (!!) xs . (`mod` length xs)

propExprOptimizeSmaller :: Expr -> Bool
propExprOptimizeSmaller expr = exprComplexity expr >= exprComplexity (optimizeExpr expr)
-- }}}

-- vim: set fdm=marker :
