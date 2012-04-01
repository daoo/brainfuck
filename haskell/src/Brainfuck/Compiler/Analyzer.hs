module Brainfuck.Compiler.Analyzer where

import Brainfuck.Compiler.IL

data Temp = TempLoop [Temp]
          | Delta Int
  deriving (Show)

memoryRequired :: [IL] -> [Temp]
memoryRequired = helper
  where
    helper []                = []
    helper (Loop loop : ils) = TempLoop (helper loop) : helper ils
    helper (Poke d _ : ils)  = Delta d : helper ils
    helper (Shift s : ils)   = Delta s : helper ils
    helper (PutChar d : ils) = Delta d : helper ils
    helper (GetChar d : ils) = Delta d : helper ils

