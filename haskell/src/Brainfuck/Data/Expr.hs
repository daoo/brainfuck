{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import qualified Data.IntMap as M

type Constant  = Int
type Variable  = Int
type Multiple  = (Int, Variable)
type Variables = M.IntMap Int

type Expr = (Constant, Variables)

variable :: Int -> Int -> Expr
variable d n = (0, M.singleton d n)

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

-- |Check if the expression is a single variable
varAnalysis :: Expr -> Maybe Multiple
varAnalysis e = case M.assocs $ snd e of
  [(d, n)] -> Just (d, n)
  _        -> Nothing

showExpr :: Expr -> String
showExpr = uncurry (M.foldrWithKey'
  (\d n -> shows n . showString "#" . shows d . showString " + ") . show)

mapVars :: (M.IntMap Int -> M.IntMap Int) -> Expr -> Expr
mapVars = fmap

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 (c1, vars1) e2@(c2, vars2) = case M.lookup d1 vars2 of
  Nothing -> e2
  Just n  -> (n * c1 + c2, M.unionWith (\a b -> a * n + b) vars1 vars2)

eval :: (Int -> Int) -> Expr -> Int
eval f = uncurry (M.foldrWithKey' (\d n s -> n * f d + s))
