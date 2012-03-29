module Brainfuck.Compiler.Analyzer where

import Brainfuck.Compiler.IL

memoryRequired :: [IL] -> Int
memoryRequired = sum . map f
  where
    f il = case il of
      Loop loop            -> memoryRequired loop
      Shift (ShiftRight s) -> s
      _                    -> 0
