module Brainfuck.Compiler.Optimize where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL

-- Inline and apply instructions
applyIL :: [IL] -> [IL]
applyIL []                  = []
applyIL (Loop i loop : ils) = Loop i (applyIL loop) : applyIL ils
applyIL (il1 : il2 : ils)   = case (il1, il2) of
  -- Inline sets
  (Set d1 e1, Set d2 e2) | d2 == d1                  -> applyIL $ Set d2 (inline d1 e1 e2) : ils
                         | not (d2 `exprDepends` e1) -> Set d2 (inline d1 e1 e2) : applyIL (il1 : ils)

  (Set d1 e1, PutChar e2) -> PutChar (inline d1 e1 e2) : applyIL (il1 : ils)

  -- Apply shifts
  (Shift s1, Shift s2)   -> applyIL $ Shift (s1 + s2) : ils
  (Shift s, Set d e)     -> Set (d + s) (modifyPtr (+s) e) : applyIL (il1 : ils)
  (Shift s, Loop d loop) -> Loop (d + s) (mapIL (modifyOffset (+s)) loop) : applyIL (il1 : ils)
  (Shift s, PutChar e)   -> PutChar (modifyPtr (+s) e) : applyIL (il1 : ils)

  _ -> il1 : applyIL (il2 : ils)

applyIL (il : ils) = il : applyIL ils

-- Reduce multiplications and clear loops
reduceLoops :: [IL] -> [IL]
reduceLoops []                       = []
reduceLoops (il@(Loop d loop) : ils) = case copyLoop il of
  Nothing -> Loop d (reduceLoops loop) : reduceLoops ils
  Just xs -> map f xs ++ [Set d $ Const 0] ++ reduceLoops ils
    where f (ds, v) = Set ds $ Get ds `Add` (Const v `Mul` Get d)
reduceLoops (il : ils) = il : reduceLoops ils

-- Remove side effect free instructions from the end
removeFromEnd :: [IL] -> [IL]
removeFromEnd = reverse . helper . reverse
  where
    sideEffect (PutChar _) = True
    sideEffect _           = False

    helper []         = []
    helper (il : ils) = case il of
      Loop _ loop -> if any sideEffect loop
        then il : ils
        else helper ils
      _ -> if sideEffect il
        then il : ils
        else helper ils

-- Optimize expressions
optimizeExpressions :: IL -> IL
optimizeExpressions il = case il of
  Set d e   -> Set d $ optimizeExpr e
  PutChar e -> PutChar $ optimizeExpr e
  _         -> il

-- Remove instructions that does not do anything
clean :: IL -> Bool
clean il = case il of
  Shift s         -> s /= 0
  Set o1 (Get o2) -> o1 /= o2
  _               -> True

