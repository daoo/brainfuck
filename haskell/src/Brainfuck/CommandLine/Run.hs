module Brainfuck.CommandLine.Run where

import Data.Foldable

import Brainfuck.Compiler.IL
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser

brainfuck :: String -> String -> String
brainfuck str inp = toList $ fmap chrCell out
  where
    il = compile $ parse str
    State _ out _ = run il (newState inp)

