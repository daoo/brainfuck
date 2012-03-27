module Brainfuck.Run where

import Data.Foldable (toList)
import Data.Word

import Brainfuck.Compiler.IL
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser

brainfuck :: String -> String -> String
brainfuck str inp = map chrIntegral $ brainfuck1 str inp

brainfuck1 :: String -> String -> [Word8]
brainfuck1 str inp = toList out
  where
    il            = compile $ parse str
    State _ out _ = run (newState inp) il

