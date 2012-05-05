module Brainfuck.Compiler.Optimizing where

import Brainfuck.Compiler.IL

-- Remove instructions from the end that does not perform any side effects
removeFromEnd :: [IL] -> [IL]
removeFromEnd = reverse . helper . reverse
  where
    helper (Poke _ _ : ils) = helper ils
    helper (Shift _ : ils)  = helper ils
    helper ils              = ils

-- Remove shifts and pokes that does nothing
clean :: IL -> Bool
clean (Shift s)  = s /= 0
clean (Poke _ i) = i /= 0
clean _          = True

-- This is essentially bubble sort, could be a lot faster
sortPokes :: IL -> IL -> Action
sortPokes p1@(Poke d1 _) p2@(Poke d2 _) | d1 > d2 = Replace [p2, p1]
sortPokes _ _                                     = Keep

-- Move shifts to the end of each block
shiftShifts :: IL -> IL -> Action
shiftShifts s@(Shift sc) il = case il of
  Poke d i  -> Replace [Poke (d + sc) i, s]
  PutChar d -> Replace [PutChar (d + sc), s]
  GetChar d -> Replace [GetChar (d + sc), s]
  _         -> Keep
shiftShifts _ _ = Keep

-- Apply shifts into loops
applyShifts :: IL -> IL -> Action
applyShifts s@(Shift sc) loop@(Loop i ils) =
  Replace [Loop (i + sc) $ mapIL (modifyRelative (+sc)) ils, s]
applyShifts _ _ = Keep

-- Merge pokes and shifts that are next to eachother
mergeSame :: IL -> IL -> Action
mergeSame (Poke d1 i1) (Poke d2 i2) | d1 /= d2  = Keep
                                    | i' == 0   = Remove
                                    | otherwise = Replace [Poke d1 i']
  where i' = i1 + i2

mergeSame (Shift s1) (Shift s2) | c' == 0   = Remove
                                | otherwise = Replace [Shift c']
  where c' = s1 + s2

mergeSame _ _ = Keep

data Action = Keep | Replace [IL] | Remove
  deriving (Show)

merge2 :: (IL -> IL -> Action) -> [IL] -> [IL]
merge2 f (Loop i loop : ils) = Loop i (merge2 f loop) : merge2 f ils
merge2 f (il1 : il2 : ils)   = case f il1 il2 of
  Replace ils' -> merge2 f $ ils' ++ ils
  Keep         -> il1 : merge2 f (il2 : ils)
  Remove       -> merge2 f ils
merge2 _ ils = ils

merge3 :: (IL -> IL -> IL -> Action) -> [IL] -> [IL]
merge3 _ []                      = []
merge3 _ [il]                    = [il]
merge3 _ [il1, il2]              = [il1, il2]
merge3 f (Loop i loop : ils)     = Loop i (merge3 f loop) : merge3 f ils
merge3 f (il1 : il2 : il3 : ils) = case f il1 il2 il3 of
  Replace ils' -> merge3 f $ ils' ++ ils
  Keep         -> il1 : merge3 f (il2 : il3 : ils)
  Remove       -> merge3 f ils
