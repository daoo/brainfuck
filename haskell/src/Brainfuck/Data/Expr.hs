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
  , inlineConst
  ) where

import Brainfuck.Utility
import Control.Applicative hiding (Const)
import Test.QuickCheck

-- |An expression is a sum of some multiples of variables and a constant
-- It is important that the sequence of variables are sorted by their value.
-- This is for assymptotically fast addition.
-- Note that this is similar to a sorted list of (Int, Int) tuples where the
-- terminator contains a value (the constant).
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

-- |Find a variable and return its multiple
-- Ignores the constant
findVar :: Int -> Expr -> Maybe Int
findVar _ (Const _)                 = Nothing
findVar d (Var n d' xs) | d == d'   = Just n
                        | otherwise = findVar d xs

-- |Filter the variables of an expression
-- Leaves the constant unchanged
filterVars :: ((Int, Int) -> Bool) -> Expr -> Expr
filterVars _ e@(Const _)              = e
filterVars f (Var n d xs) | f (n, d)  = Var n d (filterVars f xs)
                          | otherwise = filterVars f xs

-- |Map the variables and the constant of an expression
mapExpr :: ((Int, Int) -> (Int, Int)) -> (Int -> Int) -> Expr -> Expr
mapExpr _ g (Const c)    = Const (g c)
mapExpr f g (Var n d xs) = uncurry Var (f (n, d)) $ mapExpr f g xs

-- |Right fold variables in an expression
-- Ignores the constant
foldVarsR :: (Int -> Int -> a -> a) -> a -> Expr -> a
foldVarsR _ acc (Const _)    = acc
foldVarsR f acc (Var n d xs) = f n d $ foldVarsR f acc xs

-- |Strict left fold variables in an expression
-- Ignores the constant
foldVarsL' :: (a -> Int -> Int -> a) -> a -> Expr -> a
foldVarsL' _ acc (Const _)    = acc
foldVarsL' f acc (Var n d xs) = let acc' = f acc n d
                                 in seq acc' $ foldVarsL' f acc' xs

-- |Addition of two expressions
-- Time complexity: O(n + m), follows the additon laws:
--
-- > forall a b c: Expr
-- > a + b       = b + a
-- > a + (b + c) = (a + b) + c
-- > a + 0       = a
-- > 0 + a       = a
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

-- |Multiply an expression with a constant
mul :: Int -> Expr -> Expr
mul n = mapExpr (mapFst (*n)) (*n)

-- |Evaluate an expression using a function for resolving variables
eval :: (Int -> Int) -> Expr -> Int
eval f = foldVarsL' (\acc n d -> acc + n * f d) 0

-- |Inline the value of a variable into an expression
-- Time complexity: O(n + m)
inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d a b = case findVar d b of
  Nothing -> b
  Just n  -> mul n a `add` filterVars ((/= d) . snd) b

-- |Special case of inlineExpr when the inlined value is a constant
-- Time complexity: O(n)
inlineConst :: Int -> Int -> Expr -> Expr
inlineConst d c = go
  where
    go (Const c')                = Const c'
    go (Var n d' xs) | d == d'   = mapExpr id (+ (n * c)) xs
                     | otherwise = Var n d' $ go xs
