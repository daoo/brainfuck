module Brainfuck.Tests.Brainfuck where

import Test.QuickCheck

import Brainfuck.CommandLine.Run
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Parser.Parser
import Brainfuck.Parser.Brainfuck

instance Arbitrary Brainfuck where
  arbitrary = oneof $ map (return . BFToken) [Plus, Minus, ShiftRight, ShiftLeft, Input, Output]

propParser :: [Brainfuck] -> Bool
propParser bf = bf == (parse $ concatMap show bf)
