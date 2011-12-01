module Brainfuck.CommandLine.Run where

import Data.Foldable

import Brainfuck.Compiler.IL
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser

brainfuck :: String -> String -> String
brainfuck str inp = map chrCell $ brainfuck1 str inp

brainfuck1 :: String -> String -> [Cell]
brainfuck1 str inp = toList out
  where
    il = compile $ parse str
    State _ out _ = run il (newState inp)

