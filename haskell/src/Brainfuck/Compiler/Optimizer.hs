module Brainfuck.Compiler.Optimizer where

import Brainfuck.Compiler.IL

optimize :: [IL] -> [IL]
optimize []          = []
optimize (Loop l:xs) = Loop (optimize l) : optimize xs

optimize (Poke p1 i1 : Poke p2 i2 : xs) | p1 == p2  = Poke p1 (i1 + i2) : optimize xs
                                        | otherwise = Poke p1 i1 : Poke p2 i2 : optimize xs

optimize (Shift s1 : il : Shift s2 : xs) = 
  case s1 `shiftEq` s2 of
    Just i  -> il' i : optimize xs
    Nothing -> Shift s1 : il : Shift s2 : optimize xs
  where
    il' p = case il of
      PutChar _ -> PutChar p
      GetChar _ -> GetChar p
      Poke _ i  -> Poke p i
      x         -> x

optimize (x:xs) = x : optimize xs

-- Check if two shifts cancels each other.
-- Returns:
--   * Just i if they shift the same ammount
--       where i is the number of shifts, negative if it starts to the left and
--             positive if it starts to the right.
--   * Nothing if they do not cancel each other
shiftEq :: MemShift -> MemShift -> Maybe Int
shiftEq (ShiftLeft i1) (ShiftRight i2) | i1 == i2  = Just (-i1)
                                       | otherwise = Nothing
shiftEq (ShiftRight i1) (ShiftLeft i2) | i1 == i2  = Just i1
                                       | otherwise = Nothing
shiftEq _ _                                        = Nothing
