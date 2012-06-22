module TestProperties where

import Data.ListZipper
import Data.Sequence (empty)
import Data.Word

import Test.QuickCheck

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Inlining
import Brainfuck.Compiler.Optimize
import Brainfuck.Compiler.Target.C99
import Brainfuck.Ext
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Brainfuck
import Brainfuck.Parser.Parser

-- {{{ ListZipper
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
comp :: (Integral a) => Int -> State a -> State a -> Bool
comp i (State _ out1 m1) (State _ out2 m2) = cut i m1 == cut i m2 && out1 == out2

compareCode :: [IL] -> [IL] -> Bool
compareCode xs ys = comp s (run state xs) (run state ys)
  where
    s = let (xsMin, xsMax) = memorySize xs
            (ysMin, ysMax) = memorySize ys
         in 1 + abs xsMin + abs xsMax + abs ysMin + abs ysMax

    state :: State Word8
    state = State [1..] empty newMemory

propOptimize :: ([IL] -> [IL]) -> [IL] -> Bool
propOptimize f xs = compareCode xs (f xs)

-- TODO: Better testing

propInline :: Int -> Expr -> [IL] -> Bool
propInline d e xs = inline d e xs `compareCode` (Set d e : xs)

propOptimizeInlineZeros, propOptimizeCopies, propOptimizeClean,
  propOptimizeExpressions, propOptimizeMergeKind, propOptimizeInlining :: [IL] -> Bool

propOptimizeClean       = propOptimize $ filterIL clean
propOptimizeCopies      = propOptimize reduceCopyLoops
propOptimizeExpressions = propOptimize $ mapIL optimizeExpressions
propOptimizeInlineZeros = propOptimize inlineZeros
propOptimizeInlining    = propOptimize inlining
propOptimizeMergeKind   = propOptimize mergeKind

propOptimizeMoveShifts :: [IL] -> Bool
propOptimizeMoveShifts xs = memoryAccess xs == memoryAccess (moveShifts xs)

propOptimizeForC :: [IL] -> Bool
propOptimizeForC xs = getOutput (run state xs) == getOutput (run state (optimizeForC xs))
  where
    state :: State Word8
    state = newState ""

-- }}}
-- {{{ Loops
exCopyLoop1 :: IL
exCopyLoop1 =
  While 0
    [ Set 0 $ Get 0 `Add` Const (-1) ]

exCopyLoop2 :: IL
exCopyLoop2 =
  While 5
    [ Set 5 $ Get 5 `Add` Const (-1)
    , Set 1 $ Get 1 `Add` Const 1
    , Set 2 $ Get 2 `Add` Const 5
    , Set 3 $ Get 3 `Add` Const 10 ]

exNotCopyLoop1 :: IL
exNotCopyLoop1 =
  While 5
    [ Set 5 $ Get 5 `Add` Const (-1)
    , Set 6 $ Get 5 `Add` Const 10 ]

exShiftLoop1 :: [IL]
exShiftLoop1 =
  [ Set 0 $ Get 0 `Add` Const 10
  , Set 1 $ Const 0
  , Set 2 $ Get 2 `Add` Const 4 `Add` Get 0
  , Set 3 $ Get 3 `Add` Const 5
  , While 3 [ Shift (-1) ] ]

-- }}}
-- {{{ Expressions
propExprOptimizeTwice :: Expr -> Bool
propExprOptimizeTwice e = let e' = optimizeExpr e in e' == optimizeExpr e'

propExprEval :: Expr -> NonEmptyList Int -> Bool
propExprEval e (NonEmpty xs) = eval f e == eval f (optimizeExpr e)
  where
    f = (!!) xs . (`mod` length xs)

propExprOptimizeSmaller :: Expr -> Bool
propExprOptimizeSmaller expr = complexity expr >= complexity (optimizeExpr expr)
-- }}}

-- vim: set fdm=marker :
