module Brainfuck.Compiler.Optimize where

import Data.Set hiding (map, filter)

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL

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

-- |Inline set instructions
inlineIL :: [IL] -> [IL]
inlineIL []                = []
inlineIL (While e ys : xs) = While e (inlineIL ys) : inlineIL xs
inlineIL (If e ys : xs)    = If e (inlineIL ys) : inlineIL xs
inlineIL (x1 : x2 : xs)    = case (x1, x2) of
  (Set d1 e1, Set d2 e2)  | d1 == d2                                    -> inlineIL (Set d2 (inline d1 e1 e2) : xs)
                          | (inlWin d1 e1 xs || d1 > d2) && inlOk d2 e1 -> Set d2 (inline d1 e1 e2)  : inlineIL (x1 : xs)
  (Set d1 e1, PutChar e2) | inlWin d1 e1 xs                             -> PutChar (inline d1 e1 e2) : inlineIL (x1 : xs)
  (_, _)                                                                -> x1                        : inlineIL (x2 : xs)

  where
    inlWin d e ys = shouldInline (occurs d ys) e
    inlOk d e     = not (exprDepends d e)

inlineIL (x : xs) = x : inlineIL xs

-- |Move shift instructions
moveShifts :: [IL] -> [IL]
moveShifts []                = []
moveShifts (While e ys : xs) = While e (moveShifts ys) : moveShifts xs
moveShifts (If e ys : xs)    = If e (moveShifts ys)    : moveShifts xs
moveShifts (x1 : x2 : xs)    = case (x1, x2) of
  (Shift s1, While e ys) -> modifyPtr (+s1) (While e (moveShifts ys)) : moveShifts (x1 : xs)
  (Shift s1, If e ys)    -> modifyPtr (+s1) (If e (moveShifts ys)) : moveShifts (x1 : xs)
  (Shift s1, Shift s2)   -> moveShifts (Shift (s1 + s2) : xs)
  (Shift s1, _)          -> modifyPtr (+s1) x2 : moveShifts (x1 : xs)
  (_, _)                 -> x1 : moveShifts (x2 : xs)

moveShifts (x : xs) = x : moveShifts xs

-- |Merge equal instructions
mergeKind :: [IL] -> [IL]
mergeKind []                = []
mergeKind (While e ys : xs) = While e (mergeKind ys) : mergeKind xs
mergeKind (If e ys : xs)    = If e (mergeKind ys) : mergeKind xs
mergeKind (x1 : x2 : xs)    = case (x1, x2) of
  (Shift s1, Shift s2)                                          -> mergeKind (Shift (s1 + s2) : xs)
  (Set d1 e1, Set d2 e2)  | d1 == d2                            -> mergeKind (Set d2 (inline d1 e1 e2) : xs)
  (GetChar d1, Set d2 e2) | d1 == d2 && not (exprDepends d1 e2) -> mergeKind (Set d2 e2 : xs)

  (_, _) -> x1 : mergeKind (x2 : xs)

mergeKind (x : xs) = x : mergeKind xs

-- |Inline initial zeroes
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
reduceCopyLoops []                      = []
reduceCopyLoops (While (Get d) ys : xs) = case copyLoop d ys of
  Nothing  -> While (Get d) (reduceCopyLoops ys) : reduceCopyLoops xs
  Just ys' -> map f ys' ++ [Set d $ Const 0] ++ reduceCopyLoops xs
    where
      f (ds, v) = Set ds $ Get ds `Add` (Const v `Mul` Get d)
reduceCopyLoops (il : ils) = il : reduceCopyLoops ils

-- |Remove side effect free instructions from the end
removeFromEnd :: [IL] -> [IL]
removeFromEnd = reverse . helper . reverse
  where
    sideEffect (PutChar _) = True
    sideEffect (While _ _) = True -- TODO: Not always a side effect
    sideEffect (If _ _)    = True -- TODO: Not always a side effect
    sideEffect _           = False

    helper []                         = []
    helper (il : ils) | sideEffect il = il : ils
                      | otherwise     = helper ils

whileToIf :: [IL] -> [IL]
whileToIf []                      = []
whileToIf (While (Get d) ys : xs) = case setToZero d ys of
  Nothing  -> While (Get d) (whileToIf ys) : whileToIf xs
  Just ys' -> If (Get d) ys' : Set d (Const 0) : whileToIf xs
whileToIf (x : xs) = x : whileToIf xs
