module Brainfuck.Compiler.Optimizer where

import Brainfuck.Compiler.IL

--optimize (s1@(Shift _) : il : s2@(Shift _) : ils) = reduceShifts s1 il s2 ++ optimize ils

optimize :: [IL] -> [IL]
optimize = shiftShifts . clean . sortPokes . reduceShifts . mergeSame

optimizeFully :: [IL] -> [IL]
optimizeFully ils | ils == ils' = ils'
                  | otherwise   = optimizeFully ils'
  where
    ils' = optimize ils

clean :: [IL] -> [IL]
clean = filterIL f
  where
    f (Shift ms) = shiftCount ms /= 0
    f (Poke _ i) = i /= 0
    f _          = True

sortPokes :: [IL] -> [IL]
sortPokes = merge2 f
  where
    f p1@(Poke d1 _) p2@(Poke d2 _) | d1 <= d2  = Keep
                                    | otherwise = Replace [p2, p1]
    f _ _ = Keep

shiftShifts :: [IL] -> [IL]
shiftShifts = merge2 f
  where
    f s@(Shift ms) (Poke d i)  = Replace [Poke (d + shiftCount ms) i, s]
    f s@(Shift ms) (PutChar d) = Replace [PutChar (d + shiftCount ms), s]
    f s@(Shift ms) (GetChar d) = Replace [GetChar (d + shiftCount ms), s]
    f _ _                      = Keep

mergeSame :: [IL] -> [IL]
mergeSame = merge2 f
  where
    f (Poke d1 i1) (Poke d2 i2) | d1 /= d2  = Keep
                                | i' == 0   = Remove
                                | otherwise = Replace [Poke d1 i']
      where
        i' = i1 + i2

    f (Shift s1) (Shift s2) | c' == 0   = Remove
                            | otherwise = Replace [Shift $ fromInt c']
      where
        c' = shiftCount s1 + shiftCount s2

    f _ _ = Keep

reduceShifts :: [IL] -> [IL]
reduceShifts = merge3 f
  where
    f (Shift s1) il (Shift s2) = if s1 `shiftEq` s2
      then Replace [modifyRelative il 0]
      else Keep
    f _ _ _ = Keep

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
