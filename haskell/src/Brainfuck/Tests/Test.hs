module Brainfuck.Tests.Test where

import Test.QuickCheck

import Brainfuck.Compiler.IL (compile, decompile)
import Brainfuck.Ext
import Brainfuck.Parser.Brainfuck
import Brainfuck.Parser.Parser

propShiftLength :: ([a], [a]) -> NonNegative Int -> NonNegative Int -> Bool
propShiftLength x (NonNegative l) (NonNegative r) = f x1 == f x2
  where
    -- Hack to save time :/
    l' = l `mod` 1000
    r' = r `mod` 1000

    x1 = times shiftL x l'
    x2 = times shiftR x r'

    f (a, b) = length a + length b


propShiftToEmpty :: ([a], [a]) -> Bool
propShiftToEmpty x@(a, b) = null ae && null be && length af == lx && length bf == lx
  where
    la = length a
    lb = length b

    lx = la + lb

    (ae, bf) = times shiftR x la
    (af, be) = times shiftL x lb

instance Arbitrary Token where
  arbitrary = oneof $ map return [Plus, Minus, ShiftRight, ShiftLeft, Input, Output]

instance Arbitrary Brainfuck where
  arbitrary = Token `fmap` arbitrary

propParser :: [Brainfuck] -> Bool
propParser bf = bf == parse (show bf)
