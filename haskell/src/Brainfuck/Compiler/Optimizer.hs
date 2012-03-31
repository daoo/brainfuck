module Brainfuck.Compiler.Optimizer where

import Brainfuck.Compiler.IL

optimize :: [IL] -> [IL]
optimize = merge3 reduceShifts
         . merge2 sortPokes
         . filterIL clean
         . merge2 mergeSame
         . whileModified (merge2 shiftShifts)

whileModified :: (Eq a) => (a -> a) -> a -> a
whileModified f a | a == a'   = a'
                  | otherwise = whileModified f a'
  where a' = f a

optimizeFully :: [IL] -> [IL]
optimizeFully = removeFromEnd . whileModified optimize

removeFromEnd :: [IL] -> [IL]
removeFromEnd = reverse . helper . reverse
  where
    helper (Poke _ _ : ils) = helper ils
    helper (Shift _ : ils)  = helper ils
    helper ils              = ils

clean :: IL -> Bool
clean (Shift ms) = shiftCount ms /= 0
clean (Poke _ i) = i /= 0
clean _          = True

sortPokes :: IL -> IL -> Merge
sortPokes p1@(Poke d1 _) p2@(Poke d2 _) | d1 <= d2  = Keep
                                        | otherwise = Replace [p2, p1]
sortPokes _ _ = Keep

shiftShifts :: IL -> IL -> Merge
shiftShifts s@(Shift ms) il = let sc = shiftCount ms in case il of
  Poke d i  -> Replace [Poke (d + sc) i, s]
  PutChar d -> Replace [PutChar (d + sc), s]
  GetChar d -> Replace [GetChar (d + sc), s]
  _         -> Keep
shiftShifts _ _ = Keep

mergeSame :: IL -> IL -> Merge
mergeSame (Poke d1 i1) (Poke d2 i2) | d1 /= d2  = Keep
                                    | i' == 0   = Remove
                                    | otherwise = Replace [Poke d1 i']
  where
    i' = i1 + i2

mergeSame (Shift s1) (Shift s2) | c' == 0   = Remove
                                | otherwise = Replace [Shift $ fromInt c']
  where
    c' = shiftCount s1 + shiftCount s2

mergeSame _ _ = Keep

reduceShifts :: IL -> IL -> IL -> Merge
reduceShifts (Shift s1) il (Shift s2) = if s1 `shiftEq` s2
  then Replace [modifyRelative (const 0) il]
  else Keep
reduceShifts _ _ _ = Keep

shiftEq :: MemShift -> MemShift -> Bool
shiftEq (ShiftLeft i1) (ShiftRight i2) = i1 == i2
shiftEq (ShiftRight i1) (ShiftLeft i2) = i1 == i2
shiftEq _ _                            = False

data Merge = Keep | Replace [IL] | Remove

merge2 :: (IL -> IL -> Merge) -> [IL] -> [IL]
merge2 f (Loop loop : ils) = Loop (merge2 f loop) : merge2 f ils
merge2 f (il1 : il2 : ils) = case f il1 il2 of
  Replace ils' -> merge2 f $ ils' ++ ils
  Keep         -> il1 : merge2 f (il2 : ils)
  Remove       -> merge2 f ils
merge2 _ ils = ils

merge3 :: (IL -> IL -> IL -> Merge) -> [IL] -> [IL]
merge3 _ []                      = []
merge3 _ [il]                    = [il]
merge3 _ [il1, il2]              = [il1, il2]
merge3 f (Loop loop : ils)       = Loop (merge3 f loop) : merge3 f ils
merge3 f (il1 : il2 : il3 : ils) = case f il1 il2 il3 of
  Replace ils' -> merge3 f $ ils' ++ ils
  Keep         -> il1 : merge3 f (il2 : il3 : ils)
  Remove       -> merge3 f ils
