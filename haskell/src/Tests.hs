module Tests where

import Data.Word

import Test.QuickCheck

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimize
import Brainfuck.Ext
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State

import qualified Brainfuck.Parser.Brainfuck as B
import Brainfuck.Parser.Parser

-- {{{ Memory operations
propShiftLength :: ([a], [a]) -> NonNegative Int -> NonNegative Int -> Bool
propShiftLength x (NonNegative l) (NonNegative r) = f x1 == f x2
  where
    -- Hack to save time :/
    l' = l `mod` 1000
    r' = r `mod` 1000

    x1 = times shiftL l' x
    x2 = times shiftR r' x

    f (a, b) = length a + length b

propShiftToEmpty :: ([a], [a]) -> Bool
propShiftToEmpty x@(a, b) = null ae && null be && length af == lx && length bf == lx
  where
    la = length a
    lb = length b

    lx = la + lb

    (ae, bf) = times shiftR la x
    (af, be) = times shiftL lb x
-- }}}
-- {{{ Parser
propParser :: [B.Brainfuck] -> Bool
propParser bf = bf == parse (show bf)
-- }}}
-- {{{ Optimization
comp :: (Integral a) => Int -> State a -> State a -> Bool
comp i (State _ _ (ml1, mr1)) (State _ _ (ml2, mr2)) = 
  take i ml1 == take i ml2 && take i mr1 == take i mr2

propOptimize :: ([IL] -> [IL]) -> [IL] -> Bool
propOptimize f il = comp (length il) (run state il) (run state opt)
  where
    state :: State Word8
    state = newState ""
    opt   = f il

propOptimizeClean, propOptimizeExpressions, propOptimizeApply,
  propOptimizeLoops :: [IL] -> Bool
propOptimizeLoops       = propOptimize reduceLoops
propOptimizeApply       = propOptimize applyIL
propOptimizeClean       = propOptimize $ filterIL clean
propOptimizeExpressions = propOptimize $ mapIL optimizeExpressions
-- }}}
-- {{{ Copy Loops
exCopyLoop1 :: IL
exCopyLoop1 =
  Loop 0
    [ Set 0 $ Get 0 `Add` Const (-1) ]

exCopyLoop2 :: IL
exCopyLoop2 =
  Loop 5
    [ Set 5 $ Get 5 `Add` Const (-1)
    , Set 1 $ Get 1 `Add` Const 1
    , Set 2 $ Get 2 `Add` Const 5
    , Set 3 $ Get 3 `Add` Const 10 ]

exNotCopyLoop1 :: IL
exNotCopyLoop1 =
  Loop 5
    [ Set 5 $ Get 5 `Add` Const (-1)
    , Set 6 $ Get 5 `Add` Const 10 ]
-- }}}
-- {{{ Expressions
propOptimizeExprTwice :: Expr -> Bool
propOptimizeExprTwice e = let e' = optimizeExpr e in e' == optimizeExpr e'
-- }}}

-- vim: set fdm=marker :
