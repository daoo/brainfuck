module Brainfuck.Compiler.Optimizations where

import Brainfuck.Compiler.IL

mergeSame :: IL -> IL -> [IL]
mergeSame (Shift s1) (Shift s2)                = undefined
mergeSame (Poke p1 i1) (Poke p2 i2) | p1 == p2 = [Poke p1 (i1 + i2)]
mergeSame il1 il2                              = [il1, il2]

reduceShifts :: IL -> IL -> IL -> [IL]
reduceShifts (Shift s1) il (Shift s2) = case s1 `shiftEq` s2 of
  Just i  -> [modifyRelative il i]
  Nothing -> [Shift s1, il, Shift s2]
reduceShifts il1 il2 il3 = [il1, il2, il3]

modifyRelative :: IL -> Int -> IL
modifyRelative (PutChar _) p = PutChar p
modifyRelative (GetChar _) p = GetChar p
modifyRelative (Poke _ i) p  = Poke p i
modifyRelative il _          = il
     
-- Check if two shifts cancels each other.
-- Returns:
--   * Just i if the shifts cancel eachother
--       where i is the number of shifts, negative if it starts to the left and
--             positive if it starts to the right.
--   * Nothing if they do not cancel each other
shiftEq :: MemShift -> MemShift -> Maybe Int
shiftEq (ShiftLeft i1) (ShiftRight i2) | i1 == i2 = Just (-i1)
shiftEq (ShiftRight i1) (ShiftLeft i2) | i1 == i2 = Just i1
shiftEq _ _                                       = Nothing
