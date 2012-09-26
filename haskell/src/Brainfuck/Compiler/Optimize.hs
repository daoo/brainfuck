module Brainfuck.Compiler.Optimize where

import Brainfuck.Compiler.Analyzer
import Brainfuck.Compiler.Inlining
import Brainfuck.Data.Expr
import Brainfuck.Data.IL
import Brainfuck.Ext
import Data.Maybe
import qualified Data.Set as S

optimizeAll :: [IL] -> [IL]
optimizeAll = removeFromEnd . whileModified pipeline
  where
    pipeline = cleanUp
             . whileToIf
             . reduceCopyLoops
             . moveShifts
             . mapIL optimizeExpressions
             . inlining
             . inlineZeros

-- Optimize expressions
optimizeExpressions :: IL -> IL
optimizeExpressions il = case il of
  While e xs -> While (optimizeExpr e) xs
  If e xs    -> If (optimizeExpr e) xs
  Set d e    -> Set d $ optimizeExpr e
  PutChar e  -> PutChar $ optimizeExpr e
  _          -> il

-- Remove instructions that does not do anything
cleanUp :: [IL] -> [IL]
cleanUp []       = []
cleanUp (x : xs) = case x of
  While (Const i) ys | i == 0    -> cleanUp xs
                     | otherwise -> While (Const 1) (cleanUp ys) : cleanUp xs -- never ending loop
  While e ys                     -> While e (cleanUp ys) : cleanUp xs

  If (Const i) ys | i == 0    -> cleanUp xs
                  | otherwise -> cleanUp ys ++ cleanUp xs
  If e ys                     -> If e (cleanUp ys) : cleanUp xs

  Set d1 (Get d2) | d1 == d2 -> cleanUp xs
  Shift s         | s == 0   -> cleanUp xs

  _ -> x : cleanUp xs

inlining :: [IL] -> [IL]
inlining []       = []
inlining (x : xs) = case x of
  While e ys -> While e (inlining ys) : inlining xs
  If e ys    -> If e (inlining ys) : inlining xs
  Set d e    -> fromMaybe (x : inlining xs) (optimisticInlining d e xs)
  _          -> x : inlining xs

-- |Move shift instructions
moveShifts :: [IL] -> [IL]
moveShifts []             = []
moveShifts (x1 : x2 : xs) = case x1 of
  While e ys -> While e (moveShifts ys) : moveShifts (x2 : xs)
  If e ys    -> If e (moveShifts ys)    : moveShifts (x2 : xs)

  Shift s1 -> case x2 of
    Shift s2 -> moveShifts $ Shift (s1 + s2) : xs
    _        -> moveShifts $ modifyPtr (+s1) x2 : x1 : xs

  _ -> x1 : moveShifts (x2 : xs)

moveShifts (x : xs) = x : moveShifts xs

-- |Inline initial zeroes
inlineZeros :: [IL] -> [IL]
inlineZeros = go S.empty
  where
    go :: S.Set Int -> [IL] -> [IL]
    go _ []         = []
    go s (il : ils) = case il of
      While _ _ -> il : ils
      If _ _    -> error "FIXME: inlineZeros If _ _"
      Set i e   -> Set i (inl s e) : go (S.insert i s) ils
      PutChar e -> PutChar (inl s e) : go s ils
      GetChar _ -> il : go s ils
      Shift _   -> il : ils

    inl :: S.Set Int -> Expr -> Expr
    inl = unfold Add Mul . f
      where
        f s (Get i) | S.member i s = Get i
                    | otherwise    = Const 0
        f _ e                      = e

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
