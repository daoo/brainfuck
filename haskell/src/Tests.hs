module Tests where

import Test.QuickCheck

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizer
import Brainfuck.Ext
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Brainfuck
import Brainfuck.Parser.Parser

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

propParser :: [Brainfuck] -> Bool
propParser bf = bf == parse (show bf)

comp :: (Integral a) => Int -> State a -> State a -> Bool
comp i (State _ _ (ml1, mr1)) (State _ _ (ml2, mr2)) = 
  take i ml1 == take i ml2 && take i mr1 == take i mr2

propOptimize :: ([IL] -> [IL]) -> [IL] -> Bool
propOptimize f il = comp len (run state il) (run state opt)
  where
    state = newState ""
    opt   = f il
    len   = length il

propOptimizeClean        = propOptimize clean
propOptimizeSortPokes    = propOptimize sortPokes
propOptimizeShiftShifts  = propOptimize shiftShifts
propOptimizeMergeSame    = propOptimize mergeSame
propOptimizeReduceShifts = propOptimize reduceShifts
propOptimizeFully        = propOptimize optimizeFully
