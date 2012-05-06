module Brainfuck.Compiler.Optimizing where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.IL

-- Remove side effect free instructions from the end
removeFromEnd :: [IL] -> [IL]
removeFromEnd = reverse . helper . reverse
  where
    helper []         = []
    helper (il : ils) = case il of
      PutChar _ -> il : ils
      Loop _ _  -> il : ils
      _         -> helper ils

-- Remove shifts and pokes that does nothing
clean :: IL -> Bool
clean (Shift s)  = s /= 0
clean (Poke _ i) = i /= 0
clean _          = True

-- This is essentially bubble sort, could be a lot faster
sortMutators :: IL -> IL -> Action
sortMutators i1 i2 = case (i1, i2) of
  (Poke d1 _, AddFrom d2 d3) | d1 > d2 && d1 /= d3 -> Replace [i2, i1]
  (Poke d1 _, Poke d2 _)     | d1 > d2             -> Replace [i2, i1]
  (Poke d1 _, Set d2 _)      | d1 > d2             -> Replace [i2, i1]
  (Set d1 _, Poke d2 _)      | d1 > d2             -> Replace [i2, i1]
  (Set d1 _, Set d2 _)       | d1 > d2             -> Replace [i2, i1]
  _                                                -> Keep

-- Move shifts to the end of each block
shiftShifts :: IL -> IL -> Action
shiftShifts s@(Shift sc) il = case il of
  Poke d i      -> Replace [Poke (d + sc) i, s]
  PutChar d     -> Replace [PutChar (d + sc), s]
  GetChar d     -> Replace [GetChar (d + sc), s]
  Set d v       -> Replace [Set (d + sc) v, s]
  AddFrom d1 d2 -> Replace [AddFrom (d1 + sc) (d2 + sc), s]
  _             -> Keep
shiftShifts _ _ = Keep

-- Apply shifts into loops
applyShifts :: IL -> IL -> Action
applyShifts s@(Shift sc) (Loop i ils) =
  Replace [Loop (i + sc) $ mapIL (modifyRelative (+sc)) ils, s]
applyShifts _ _ = Keep

-- Reduce some loops to simpler operations
reduceLoops :: IL -> Action
reduceLoops il = case il of
  Loop dl [Poke dp (-1)] | dl == dp -> Replace [Set dp 0]

  Loop _ _ -> case copyLoop il of
    Nothing      -> Keep
    Just (o, xs) -> Replace $ map (`AddFrom` o) xs ++ [Set o 0]

  _ -> Keep

-- Merge pokes and shifts that are next to eachother
mergeSame :: IL -> IL -> Action
mergeSame il1 il2 = case (il1, il2) of
  (Poke d1 i1, Poke d2 i2) | d1 /= d2  -> Keep
                           | i' == 0   -> Remove
                           | otherwise -> Replace [Poke d1 i']
    where i' = i1 + i2

  (Shift s1, Shift s2) | c' == 0   -> Remove
                       | otherwise -> Replace [Shift c']
    where c' = s1 + s2

  (Set s1 _, Set s2 v)      | s1 == s2 -> Replace [Set s1 v]
  (Set s1 0, AddFrom c1 c2) | s1 == c1 -> Replace [SetFrom c1 c2]

  (_, _) -> Keep

data Action = Keep | Replace [IL] | Remove
  deriving (Show)

merge1 :: (IL -> Action) -> [IL] -> [IL]
merge1 f (Loop d loop : ils) = case f (Loop d (merge1 f loop)) of
  Replace ils' -> merge1 f $ ils' ++ ils
  Keep         -> Loop d (merge1 f loop) : merge1 f ils
  Remove       -> merge1 f ils
merge1 f (il : ils)          = case f il of
  Replace ils' -> merge1 f $ ils' ++ ils
  Keep         -> il : merge1 f ils
  Remove       -> merge1 f ils
merge1 _ ils = ils

-- TODO: il1 can not be a loop
merge2 :: (IL -> IL -> Action) -> [IL] -> [IL]
merge2 f (Loop d loop : ils) = Loop d (merge2 f loop) : merge2 f ils
merge2 f (il1 : il2 : ils)   = case f il1 il2 of
  Replace ils' -> merge2 f $ ils' ++ ils
  Keep         -> il1 : merge2 f (il2 : ils)
  Remove       -> merge2 f ils
merge2 _ ils = ils

-- TODO: il2 can not be a loop
merge3 :: (IL -> IL -> IL -> Action) -> [IL] -> [IL]
merge3 f (Loop d loop : ils)     = Loop d (merge3 f loop) : merge3 f ils
merge3 f (il1 : il2 : il3 : ils) = case f il1 il2 il3 of
  Replace ils' -> merge3 f $ ils' ++ ils
  Keep         -> il1 : merge3 f (il2 : il3 : ils)
  Remove       -> merge3 f ils
merge3 _ ils = ils
