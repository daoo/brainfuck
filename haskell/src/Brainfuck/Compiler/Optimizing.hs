module Brainfuck.Compiler.Optimizing where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL

inline :: [IL] -> [IL]
inline []                  = []
inline (Loop i loop : ils) = Loop i (inline loop) : inline ils
inline (il1 : il2 : ils)   = case (il1, il2) of
  -- Inline sets
  (Set d1 e1, Set d2 e2) | d2 == d1                  -> Set d2 (inlineSet d1 e1 e2) : inline ils
                         | not (e1 `exprDepends` d2) -> Set d2 (inlineSet d1 e1 e2) : inline (il1 : ils)

  (Set d1 e1, PutChar e2) -> PutChar (inlineSet d1 e1 e2) : inline (il1 : ils)

  -- Apply shifts
  (Shift s1, Shift s2)   -> inline $ Shift (s1 + s2) : ils
  (Shift s, Set d e)     -> Set (d + s) (modifyPtr (+s) e) : inline (il1 : ils)
  (Shift s, Loop d loop) -> Loop (d + s) (mapIL (modifyOffset (+s)) loop) : (il1 : ils)

  _ -> il1 : inline (il2 : ils)

inline (il : ils) = il : inline ils

-- Reduce multiplications and clear loops
reduceLoops :: [IL] -> [IL]
reduceLoops []                  = []
reduceLoops (il@(Loop d loop) : ils) = case copyLoop il of
  Nothing -> Loop d (reduceLoops loop) : reduceLoops ils
  Just xs -> map (\(ds, v) -> Set ds (Const v `Mult` Get d)) xs ++ [Set d (Const 0)] ++ reduceLoops ils
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
  Set d e -> Set d $ cleanExpr e
  _       -> il

-- Remove instructions that does not do anything
clean :: IL -> Bool
clean il = case il of
  Shift s         -> s /= 0
  Set o1 (Get o2) -> o1 /= o2
  _               -> True

