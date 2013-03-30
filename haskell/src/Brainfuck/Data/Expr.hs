{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import qualified Data.IntMap as M

type Constant  = Int
type Variable  = Int
type Multiple  = (Variable, Int)
type Variables = M.IntMap Int

type Expr = (Constant, Variables)

variable :: Int -> Expr
variable d = (0, M.singleton d 1)

variables :: [Multiple] -> Expr
variables = (,) 0 . M.fromList

constant :: Constant -> Expr
constant = flip (,) M.empty

add :: Expr -> Expr -> Expr
add (c1, v1) (c2, v2) = (c1 + c2, M.unionWith (+) v1 v2)

-- |Check if the expression is const only
constAnalysis :: Expr -> Maybe Int
constAnalysis (c, v) | M.null v  = Just c
                     | otherwise = Nothing

-- |Match the expression with 0 + (1 * some variable)
varAnalysis :: Expr -> Maybe Int
varAnalysis (0, v) = case M.assocs v of
  [(d, 1)] -> Just d
  _        -> Nothing

varAnalysis _ = Nothing

showExpr :: Expr -> String
showExpr = uncurry (M.foldrWithKey'
  (\d n -> shows n . showString "#" . shows d . showString " + ") . show)

mapVars :: (M.IntMap Int -> M.IntMap Int) -> Expr -> Expr
mapVars = fmap

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 (c1, vars1) e2@(c2, vars2) = case M.lookup d1 vars2 of
  Nothing -> e2
  Just n  -> (n * c1 + c2, inline d1 n vars1 vars2)

  where
    inline d n l r = M.mergeWithKey (f d n) (M.map (*n)) (M.filterWithKey (\d' _ -> d /= d')) l r

    f d m d' nl nr | d == d'   = Nothing
                   | otherwise = Just (m * nl + nr)

eval :: (Int -> Int) -> Expr -> Int
eval f = uncurry (M.foldrWithKey' (\d n s -> n * f d + s))
