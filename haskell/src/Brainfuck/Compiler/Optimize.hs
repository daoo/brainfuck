module Brainfuck.Compiler.Optimize where

import Data.Set hiding (map, filter)

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL

-- Inline and apply instructions
applyIL :: [IL] -> [IL]
applyIL []                = []
applyIL (While d ys : xs) = While d (applyIL ys) : applyIL xs
applyIL (If e ys : xs)    = If e (applyIL ys) : applyIL xs
applyIL (il1 : il2 : ils) = case il1 of
  Set d1 e1 -> case il2 of

    Set d2 _   | d2 == d1                        -> applyIL $ il2' : ils
               | (inlWin || d1 > d2) && inlOk d2 -> il2'           : applyIL (il1  : ils)
    PutChar _  | inlWin                          -> il2'           : applyIL (il1  : ils)
    GetChar d2 | d1 == d2                        -> GetChar d2     : applyIL ils

    _ -> il1 : applyIL (il2 : ils)
    where
      inlWin  = shouldInline (occurrs d1 ils) e1
      inlOk d = not (exprDepends d e1)

      il2' = inl il2

      inl (Set d2 e2)  = Set d2 (inline d1 e1 e2)
      inl (PutChar e2) = PutChar (inline d1 e1 e2)
      inl il           = il

  Shift s1 -> case il2 of

    Shift s2   -> applyIL $ Shift (s1 + s2)                              : ils
    Set d e    -> Set (d + s1) (modifyPtr (+s1) e)                       : applyIL (il1 : ils)
    While d ys -> While (d + s1) (mapIL (modifyOffset (+s1)) ys)         : applyIL (il1 : ils)
    If e ys    -> If (modifyPtr (+s1) e) (mapIL (modifyOffset (+s1)) ys) : applyIL (il1 : ils)
    PutChar e  -> PutChar (modifyPtr (+s1) e)                            : applyIL (il1 : ils)
    GetChar d2 -> GetChar (d2 + s1)                                      : applyIL (il1 : ils)

  _ -> il1 : applyIL (il2 : ils)

applyIL (il : ils) = il : applyIL ils

-- |Inline instructions
-- Inline initial zeros
inlineZeros :: [IL] -> [IL]
inlineZeros = go empty
  where
    go :: Set Int -> [IL] -> [IL]
    go _ []         = []
    go s (il : ils) = case il of
      While _ _ -> il : ils
      If _ _    -> il : ils -- FIXME: We could maybe inline zeroes into ifs
      Set i e   -> Set i (inl s e) : go (insert i s) ils
      PutChar e -> PutChar (inl s e) : go s ils
      GetChar _ -> il : go s ils
      Shift _   -> il : ils

    inl :: Set Int -> Expr -> Expr
    inl = unfold Add Mul . f
      where
        f s (Get i) | member i s = Get i
                    | otherwise  = Const 0
        f _ e = e

-- |Reduce multiplications and clear loops
reduceCopyLoops :: [IL] -> [IL]
reduceCopyLoops []                = []
reduceCopyLoops (While d ys : xs) = case copyLoop d ys of
  Nothing  -> While d (reduceCopyLoops ys) : reduceCopyLoops xs
  Just ys' -> map f ys' ++ [Set d $ Const 0] ++ reduceCopyLoops xs
    where
      f (ds, v) = Set ds $ Get ds `Add` (Const v `Mul` Get d)
reduceCopyLoops (il : ils) = il : reduceCopyLoops ils

-- |Reduce shift loops
reduceShiftLoops :: [IL] -> [IL]
reduceShiftLoops = go []
  where
    go :: [Int] -> [IL] -> [IL]
    go _ []            = []
    go zeroes (x : xs) = case x of
      Set d (Const 0)                        -> x : go (d : zeroes) xs
      Set d (Const _)                        -> x : go (filter (/= d) zeroes) xs
      Shift d                                -> x : go (map (d -) zeroes) xs
      While d1 [Shift d2] | d1 `elem` zeroes -> go zeroes $ While (d1 + d2) [Shift d2] : xs
      _                                      -> x : go zeroes xs

-- |Remove side effect free instructions from the end
removeFromEnd :: [IL] -> [IL]
removeFromEnd = reverse . helper . reverse
  where
    sideEffect (PutChar _) = True
    sideEffect (While _ _) = True
    sideEffect _           = False

    helper []                         = []
    helper (il : ils) | sideEffect il = il : ils
                      | otherwise     = helper ils

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

