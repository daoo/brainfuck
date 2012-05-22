module TestProperties where

import Data.Word
import Data.ListZipper

import Test.QuickCheck

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimize
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

propOptimize :: ([IL] -> [IL]) -> [IL] -> Property
propOptimize f ils = isJust s ==>
  comp (fromJust s) (run state ils) (run state (f ils))
  where
    s = (\(mn, mx) -> abs mn + abs mx) `fmap` memorySize ils

    state :: State Word8
    state = newState ""

propOptimizeInlineZeros, propOptimizeCopies, propOptimizeShifts,
  propOptimizeApply, propOptimizeClean, propOptimizeExpressions :: [IL] -> Property

propOptimizeInlineZeros = propOptimize inlineZeros
propOptimizeCopies      = propOptimize reduceCopyLoops
propOptimizeShifts      = propOptimize reduceShiftLoops
propOptimizeApply       = propOptimize applyIL
propOptimizeClean       = propOptimize $ filterIL clean
propOptimizeExpressions = propOptimize $ mapIL optimizeExpressions
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
propExprEval e (NonEmpty xs) = eval f e == eval f e'
  where
    e' = optimizeExpr e
    l  = length xs

    f :: Int -> Int
    f i = xs !! (i `mod` l)

propExprOptimizeSmaller :: Expr -> Bool
propExprOptimizeSmaller expr = complexity expr >= complexity (optimizeExpr expr)
-- }}}

-- vim: set fdm=marker :
