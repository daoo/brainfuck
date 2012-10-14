{-# LANGUAGE LambdaCase #-}
module Brainfuck.Compiler.Optimize where

import Brainfuck.Compiler.Analysis
import Brainfuck.Data.Expr
import Brainfuck.Data.IL
import Brainfuck.Ext
import qualified Data.Map as M
import qualified Data.Set as S

applyHelper :: ([IL] -> [IL]) -> [IL] -> [IL]
applyHelper f = \case
  []              -> []
  If    e ys : xs -> If e (f ys)    : f xs
  While e ys : xs -> While e (f ys) : f xs
  x          : xs -> x              : f xs

optimizeAll :: [IL] -> [IL]
optimizeAll = removeFromEnd . whileModified pipeline
  where
    pipeline = id
             . whileToIf
             . reduceCopyLoops
             . moveShifts
             . mapIL optimizeExpressions
             . inlineZeros
             . movePutGet
             . cleanUp

-- |Merge sequences of Set ILs
optimizeSets :: [IL] -> [IL]
optimizeSets = \case
  xs@(Set _ _ : _) -> uncurry (++) $ mapTuple opt optimizeSets $ span isSet xs

  xs -> applyHelper optimizeSets xs

  where
    opt :: [IL] -> [IL]
    opt = map (uncurry Set) . optimalSets . map (\(Set d e) -> (d, e))

    isSet (Set _ _) = True
    isSet _         = False

-- |Optimize expressions
optimizeExpressions :: IL -> IL
optimizeExpressions = \case
  Set d e    -> Set d $ optimizeExpr e
  PutChar e  -> PutChar $ optimizeExpr e
  x          -> x

-- |Remove instructions that provides no side effects
cleanUp :: [IL] -> [IL]
cleanUp = \case
  While (Const i) ys : xs | i == 0    -> cleanUp xs
                          | otherwise -> While (Const 1) (cleanUp ys) : cleanUp xs -- never ending loop

  If (Const i) ys : xs | i == 0    -> cleanUp xs
                       | otherwise -> cleanUp ys ++ cleanUp xs

  Set d1 (Get d2) : xs | d1 == d2 -> cleanUp xs
  Shift s         : xs | s == 0   -> cleanUp xs

  xs -> applyHelper cleanUp xs

movePutGet :: [IL] -> [IL]
movePutGet = \case
  x1@(Set d e1) : x2 : xs -> case x2 of
    PutChar e2 -> PutChar (inlineExpr d e1 e2) : x1 : movePutGet xs
    _          -> x1 : movePutGet (x2 : xs)

  xs -> applyHelper movePutGet xs

-- |Move shift instructions towards the end.
-- This make most of the shift operations disappear
moveShifts :: [IL] -> [IL]
moveShifts = \case
  x1@(Shift s1) : x2 : xs -> case x2 of
    Shift s2 -> moveShifts $ Shift (s1 + s2) : xs
    _        -> moveShifts $ modifyPtr (+s1) x2 : x1 : xs

  xs -> applyHelper moveShifts xs

-- |Reduce multiplications and clear loops
reduceCopyLoops :: [IL] -> [IL]
reduceCopyLoops = \case
  While (Get d) ys : xs -> case copyLoop d ys of
    Nothing  -> While (Get d) (reduceCopyLoops ys) : reduceCopyLoops xs
    Just ys' -> map f ys' ++ [Set d $ Const 0] ++ reduceCopyLoops xs
      where
        f (ds, v) = Set ds $ Get ds `Add` (Const v `Mul` Get d)
  xs -> applyHelper reduceCopyLoops xs

-- |Convert while loops that are only run once to if statements
whileToIf :: [IL] -> [IL]
whileToIf = \case
  While e@(Get d) ys : xs -> case setToZero d ys of
    Nothing  -> While e (whileToIf ys) : whileToIf xs
    Just ys' -> If (Get d) ys' : Set d (Const 0) : whileToIf xs
  xs -> applyHelper whileToIf xs

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

-- |Calculate the optimal representation of some Set ILs
optimalSets :: [(Int, Expr)] -> [(Int, Expr)]
optimalSets = go M.empty
  where
    go :: M.Map Int Expr -> [(Int, Expr)] -> [(Int, Expr)]
    go m []          = M.assocs m
    go m ((x, e):xs) = go (M.alter (const $ Just $ f m e) x m) xs

    f :: M.Map Int Expr -> Expr -> Expr
    f m = modifyLeafs (g m)

    g :: M.Map Int Expr -> Expr -> Expr
    g m e@(Get i) = case M.lookup i m of
      Nothing -> e
      Just e' -> e'

    g _ e = e
