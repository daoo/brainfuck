{-# LANGUAGE BangPatterns, GADTs, FlexibleInstances #-}
module Brainfuck.Data.Expr
  ( Expr(..)
  , findVar
  , filterVars
  , foldVarsR
  , foldVarsL'

  , (.+)
  , (.*)
  , shiftExpr

  , evalExpr

  , insertExpr
  , insertConst
  ) where

import Control.Arrow (first, second)
import Control.Applicative ((<$>))
import Test.QuickCheck

-- |An expression is a sum of multiples of variables and an constant.
-- Represented as a sorted list with the constant stored in the terminator
-- which allows for linear time addition of two expressions.
data Expr n v where
  Var   :: !n -> !v -> Expr n v -> Expr n v
  Const :: !n -> Expr n v
  deriving Show

instance (Eq n, Eq v) => Eq (Expr n v) where
  Const c1     == Const c2     = c1 == c2
  Var n1 d1 xs == Var n2 d2 ys = n1 == n2 && d1 == d2 && xs == ys
  _            == _            = False

instance Arbitrary (Expr Int Int) where
  arbitrary = sized (go 0)
    where
      go _ 0 = Const <$> arbitrary
      go i s = do
        n <- arbitrary
        v <- suchThat arbitrary (>=i)
        Var n v <$> go v (s `div` 2)

  shrink (Const c)   = map Const $ shrink c
  shrink (Var n v e) = e : map (\n' -> Var n' v e) (shrink n) ++
                           map (\v' -> Var n v' e) (shrink v)

{-# SPECIALIZE findVar :: Int -> Expr Int Int -> Maybe Int #-}
-- |Find a variable and return its multiple
-- Ignores the constant
findVar :: Eq v => v -> Expr n v -> Maybe n
findVar _ (Const _)                 = Nothing
findVar v (Var n v' xs) | v == v'   = Just n
                        | otherwise = findVar v xs

-- |Filter the variables of an expression
-- Leaves the constant unchanged
filterVars :: ((n, v) -> Bool) -> Expr n v -> Expr n v
filterVars _ e@(Const _)              = e
filterVars f (Var n v xs) | f (n, v)  = Var n v (filterVars f xs)
                          | otherwise = filterVars f xs

-- |Map the variables and the constant of an expression
mapExpr :: ((n, v) -> (m, u)) -> (n -> m) -> Expr n v -> Expr m u
mapExpr _ g (Const c)    = Const (g c)
mapExpr f g (Var n v xs) = uncurry Var (f (n, v)) $ mapExpr f g xs

-- |Right fold variables in an expression
-- Ignores the constant
foldVarsR :: (n -> v -> a -> a) -> a -> Expr n v -> a
foldVarsR _ acc (Const _)    = acc
foldVarsR f acc (Var n v xs) = f n v $ foldVarsR f acc xs

-- |Strict left fold variables in an expression
-- Ignores the constant
foldVarsL' :: (a -> n -> v -> a) -> a -> Expr n v -> a
foldVarsL' _ !acc (Const _)    = acc
foldVarsL' f !acc (Var n v xs) = foldVarsL' f (f acc n v) xs

infixl 5 .+
infixl 6 .*

{-# SPECIALIZE (.+) :: Expr Int Int -> Expr Int Int -> Expr Int Int #-}
-- |Addition of two expressions
-- Time complexity: O(n + m), follows the additon laws:
--
-- > forall a b c: Expr
-- > a + b       = b + a
-- > a + (b + c) = (a + b) + c
-- > a + 0       = a
-- > 0 + a       = a
(.+) :: (Eq n, Num n, Ord v) => Expr n v -> Expr n v -> Expr n v
(.+) (Const c1)       (Const c2)       = Const (c1 + c2)
(.+) (Var n1 d1 x')   c2@(Const _)     = Var n1 d1 (x' .+ c2)
(.+) c1@(Const _)     (Var n2 d2 y')   = Var n2 d2 (c1 .+ y')
(.+) x@(Var n1 d1 x') y@(Var n2 d2 y') = case compare d1 d2 of
  LT -> Var n1 d1        $ x' .+ y
  EQ -> app (n1 + n2) d1 $ x' .+ y'
  GT -> Var n2 d2        $ x  .+ y'

  where
    app 0 _ xs = xs
    app n v xs = Var n v xs

{-# SPECIALIZE (.*) :: Int -> Expr Int Int -> Expr Int Int #-}
-- |Multiply an expression with a constant
-- Time complexity: O(n)
(.*) :: Num n => n -> Expr n v -> Expr n v
(.*) n = mapExpr (first (*n)) (*n)

{-# SPECIALIZE shiftExpr :: Int -> Expr Int Int -> Expr Int Int #-}
-- |Shift all variables in an expression
-- Time complexity: O(n)
shiftExpr :: Num v => v -> Expr n v -> Expr n v
shiftExpr s = mapExpr (second (+s)) id

{-# SPECIALIZE evalExpr :: (Int -> Int) -> Expr Int Int -> Int #-}
-- |Evaluate an expression using a function for resolving variables
-- Time complexity: O(n) if f is constant time
evalExpr :: Num n => (v -> n) -> Expr n v -> n
evalExpr f = go 0
  where
    go !acc (Const c)    = acc + c
    go !acc (Var n v xs) = go (acc + n * f v) xs

{-# SPECIALIZE insertExpr :: Int -> Expr Int Int -> Expr Int Int -> Expr Int Int #-}
-- |Insert the value of a variable into an expression
-- Time complexity: O(n + m)
insertExpr :: (Eq n, Num n, Ord v) => v -> Expr n v -> Expr n v -> Expr n v
insertExpr v a b = case findVar v b of
  Nothing -> b
  Just n  -> (n .* a) .+ filterVars ((/= v) . snd) b

{-# SPECIALIZE insertConst :: Int -> Int -> Expr Int Int -> Expr Int Int #-}
-- |Special case of insertVariable when the value is a constant
-- Time complexity: O(n)
insertConst :: (Num n, Eq v) => v -> n -> Expr n v -> Expr n v
insertConst v c = go
  where
    go (Const c')                = Const c'
    go (Var n v' xs) | v == v'   = mapExpr id (+ (n * c)) xs
                     | otherwise = Var n v' $ go xs
