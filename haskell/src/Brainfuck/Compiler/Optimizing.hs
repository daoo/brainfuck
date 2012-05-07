module Brainfuck.Compiler.Optimizing where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
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

-- Optimize expressions
optimizeExpressions :: IL -> IL
optimizeExpressions il = case il of
  Add d e -> Add d $ cleanExpr e
  Set d e -> Set d $ cleanExpr e
  _       -> il

-- Remove instructions that does not do anything
clean :: IL -> Bool
clean (Shift s)         = s  /= 0
clean (Add _ (Const i)) = i  /= 0
clean (Set o1 (Get o2)) = o1 /= o2
clean _                 = True

-- This is essentially bubble sort, could be a lot faster
sortMutators :: IL -> IL -> Action
sortMutators i1 i2 = case (i1, i2) of
  _ -> Keep

-- Move shifts to the end of each block
shiftShifts :: IL -> IL -> Action
shiftShifts s@(Shift sc) il = case il of
  Add d e           -> Replace [Add (d + sc) (modifyPtr (+sc) e), s]
  Set d e           -> Replace [Set (d + sc) (modifyPtr (+sc) e), s]
  PutChar d         -> Replace [PutChar (d + sc), s]
  GetChar d         -> Replace [GetChar (d + sc), s]
  _                 -> Keep
shiftShifts _ _ = Keep

-- Apply shifts into loops
applyShifts :: IL -> IL -> Action
applyShifts s@(Shift sc) (Loop i ils) =
  Replace [Loop (i + sc) $ mapIL (modifyOffset (+sc)) ils, s]
applyShifts _ _ = Keep

-- Reduce some loops to simpler operations
reduceLoops :: IL -> Action
reduceLoops il = case il of
  Loop dl [Add dp (Const (-1))] | dl == dp -> Replace [Set dp $ Const 0]

  Loop _ _ -> case copyLoop il of
    Nothing      -> Keep
    Just (o, xs) -> Replace $ map f xs ++ [Set o $ Const 0]
      where f (d, i) = Add d $ Mult (Get o) (Const i)

  _ -> Keep

-- Merge pokes and shifts that are next to eachother
mergeSame :: IL -> IL -> Action
mergeSame il1 il2 = case (il1, il2) of
  (Shift d1, Shift d2)              -> Replace [Shift $ d1 + d2]
  (Add d1 e1, Add d2 e2) | d1 == d2 -> Replace [Add d1 $ cleanExpr $ e1 `Plus` e2]
  (Set d1 _, Set d2 e2)  | d1 == d2 -> Replace [Set d1 e2]

  (_, _) -> Keep

miscJoins :: IL -> IL -> Action
miscJoins il1 il2 = case (il1, il2) of
  (Set d1 e1, Add d2 e2) | d1 == d2 -> Replace [Set d1 $ cleanExpr $ e1 `Plus` e2]

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
