module Brainfuck.Compiler.Analyzer where

import Brainfuck.Compiler.IL

memoryRequired :: [IL] -> Maybe Int
memoryRequired = undefined

memoryShifts :: [IL] -> Int
memoryShifts = sum . map f
  where
    f il = case il of
      Loop l  -> memoryShifts l
      Shift s -> shiftCount s
      _       -> 0

