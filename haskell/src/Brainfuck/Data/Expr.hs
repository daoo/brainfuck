{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Brainfuck.Data.Expr
  ( Expr (..)
  , findVar
  , filterVars
  , mapExpr
  , foldVarsR
  , foldVarsL'
  , add
  , eval
  , inlineExpr
  ) where

import Brainfuck.Utility
import Control.Applicative hiding (Const)
import Test.QuickCheck

-- |An expression is a constant and a sum of multiples of variables
data Expr = Var {-# UNPACK #-} !Int {-# UNPACK #-} !Int Expr
          | Const {-# UNPACK #-} !Int
  deriving (Eq, Show)

instance Arbitrary Expr where
  arbitrary = sized (go (-10))
    where
      go _ 0 = Const <$> choose (-10, 10)
      go d n = do
        i <- choose (d, 10)
        Var <$> choose (-10, 10) <*> pure i <*> go i (n `div` 2)

findVar :: Int -> Expr -> Maybe Int
findVar _ (Const _)                 = Nothing
findVar d (Var n d' xs) | d == d'   = Just n
                        | otherwise = findVar d xs

filterVars :: ((Int, Int) -> Bool) -> Expr -> Expr
filterVars _ e@(Const _)              = e
filterVars f (Var n d xs) | f (n, d)  = Var n d (filterVars f xs)
                          | otherwise = filterVars f xs

mapExpr :: ((Int, Int) -> (Int, Int)) -> (Int -> Int) -> Expr -> Expr
mapExpr _ g (Const c)    = Const (g c)
mapExpr f g (Var n d xs) = uncurry Var (f (n, d)) $ mapExpr f g xs

foldVarsR :: (Int -> Int -> a -> a) -> a -> Expr -> a
foldVarsR _ acc (Const _)    = acc
foldVarsR f acc (Var n d xs) = f n d $ foldVarsR f acc xs

foldVarsL' :: (a -> Int -> Int -> a) -> a -> Expr -> a
foldVarsL' _ acc (Const _)    = acc
foldVarsL' f acc (Var n d xs) = let acc' = f acc n d
                                 in seq acc' $ foldVarsL' f acc' xs

add :: Expr -> Expr -> Expr
add (Const c1)       (Const c2)       = Const (c1 + c2)
add (Var n1 d1 x')   c2@(Const _)     = Var n1 d1 (add x' c2)
add c1@(Const _)     (Var n2 d2 y')   = Var n2 d2 (add c1 y')
add x@(Var n1 d1 x') y@(Var n2 d2 y') = case compare d1 d2 of
  LT -> Var n1 d1        $ add x' y
  EQ -> app (n1 + n2) d1 $ add x' y'
  GT -> Var n2 d2        $ add x y'

  where
    app 0 _ xs = xs
    app n d xs = Var n d xs

eval :: (Int -> Int) -> Expr -> Int
eval f = go 0
  where
    go acc = \case
      Const c    -> acc + c
      Var n d xs -> go (acc + n * f d) xs

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d a b = case findVar d b of
  Nothing -> b
  Just n  -> mapExpr (mapFst (*n)) (*n) a `add` filterVars ((/= d) . snd) b
