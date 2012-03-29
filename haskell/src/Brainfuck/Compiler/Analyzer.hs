module Brainfuck.Compiler.Analyzer where

import Brainfuck.Compiler.IL
import Brainfuck.Ext

memoryRequired :: [IL] -> Maybe Int
memoryRequired ils | a == b    = Just a
                   | otherwise = Nothing
  where
    (a, b) = foldl1 (zipBoth (+)) $ map helper ils

    helper il = case il of
      Loop loop            -> foldl1 (zipBoth (+)) $ map helper loop
      Shift (ShiftRight d) -> (d, d)
      Shift (ShiftLeft d)  -> (-d, 0)
      _                    -> (0, 0)
