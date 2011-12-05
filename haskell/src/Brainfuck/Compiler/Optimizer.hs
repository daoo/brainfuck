module Brainfuck.Compiler.Optimizer where

import Brainfuck.Compiler.IL

optimize :: [IL] -> [IL]
optimize []          = []
optimize (Loop l:xs) = Loop (optimize l) : optimize xs

optimize (Poke p1 i1:Poke p2 i2:xs) | p1 == p2  = Poke p1 (i1 + i2) : optimize xs
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

optimize xs = xs

shiftEq :: MemShift -> MemShift -> Maybe Int
shiftEq s1 s2 | f s1 == f s2 = Just $ f s1
              | otherwise    = Nothing
  where
    f (ShiftLeft i)  = i
    f (ShiftRight i) = i

