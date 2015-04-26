-- |This module provides the most efficient definition of an expression that is
-- a sum of multiples of variables and a constant. The constant is in this
-- module denoted with `n`, an variable is denoted by `x` and the multiples are
-- denoted by `a`.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Brainfuck.Data.Expr
  ( Expr(..)
  , pattern Zero
  , pattern Constant
  , pattern Variable1
  , pattern Add

  , findVar
  , hasVariable
  , filterVars
  , foldrExpr
  , foldlExpr'
  , foldExprM'

  , (.+)
  , (.*)
  , shiftExpr

  , evalExpr

  , insertExpr
  , insertExprs
  , insertConst

  , buildExpr
  , deconsExpr
  ) where

import Control.Applicative
import Test.QuickCheck
import qualified Data.IntMap.Strict as M

-- $setup
-- >>> import Data.Maybe (isJust)

-- |An expression is a sum of multiples of variables and an constant.
-- Represented as a sorted list with the constant stored in the terminator
-- which allows for linear time addition of two expressions.
data Expr where
  V :: !Int -> !Int -> Expr -> Expr
  C :: !Int -> Expr
  deriving Show

-- |Check the invariant for an expression.
--
-- >>> invariant (C 0)
-- True
-- >>> invariant (V 1 0 (C 1))
-- True
-- >>> invariant (V 0 0 (C 0))
-- False
-- >>> invariant (V 1 1 (V 1 1 (C 1)))
-- False
-- >>> invariant (V 1 1 (V 0 0 (C 1)))
-- False
-- >>> invariant (V 1 1 (V 1 0 (C 1)))
-- True
invariant :: Expr -> Bool
invariant (C _)                  = True
invariant (V a _ (C _))          = a /= 0
invariant (V a1 x1 e@(V _ x2 _)) = a1 /= 0 && x1 > x2 && invariant e

-- |Patterns for invariant-fulfilling expressions.
-- prop> invariant Zero
-- prop> invariant (Constant n)
-- prop> invariant (Variable1 x)
-- prop> invariant (Add x n)
pattern Zero         = C 0
pattern Constant n   = C n
pattern Variable1 x  = V 1 x (C 0)
pattern Add x n      = V 1 x (C n)

instance Eq Expr where
  C n1       == C n2       = n1 == n2
  V n1 d1 xs == V n2 d2 ys = n1 == n2 && d1 == d2 && xs == ys
  _          == _          = False

-- |Arbitrary expressions which fullfills the invariant.
--
-- prop> invariant e
-- prop> all invariant (shrink e)
instance Arbitrary Expr where
  arbitrary = sized go
    where
      go :: Int -> Gen Expr
      go 0 = C <$> arbitrary
      go i = do
        n <- suchThat arbitrary (/=0)
        v <- suchThat arbitrary (liftA2 (&&) (>=0) (<i))
        V n v <$> go v

  shrink (C c)     = map C $ shrink c
  shrink (V _ _ e) = e : shrink e

-- |Find a variable and return its multiple.
--
-- >>> let expr = V 5 5 $ V 4 4 $ V 2 2 $ V 1 1 $ C 0
-- >>> invariant expr
-- True
-- >>> findVar expr 1
-- Just 1
-- >>> findVar expr 0
-- Nothing
-- >>> findVar expr 6
-- Nothing
findVar :: Expr -> Int -> Maybe Int
findVar (C _)     _  = Nothing
findVar (V a x e) x' = case compare x x' of
  LT -> Nothing
  EQ -> Just a
  GT -> findVar e x'

-- |Check if a certain expression has an variable.
--
-- prop> hasVariable e x == isJust (findVar e x)
hasVariable :: Expr -> Int -> Bool
hasVariable (C _)     _  = False
hasVariable (V _ x e) x' = x == x' || e `hasVariable` x'

-- |Filter the variables of an expression.
--
-- Leaves the constant unchanged.
filterVars :: ((Int, Int) -> Bool) -> Expr -> Expr
filterVars _ e@(C _)               = e
filterVars f (V a x e) | f (a, x)  = V a x (filterVars f e)
                       | otherwise = filterVars f e

-- |The category theory derived fold for expressions.
--
-- prop> foldrExpr V C e == e
foldrExpr :: (Int -> Int -> a -> a) -> (Int -> a) -> Expr -> a
foldrExpr _ g (C n)     = g n
foldrExpr f g (V a x e) = f a x (foldrExpr f g e)

foldlExpr' :: (acc -> Int -> Int -> acc)
           -> (acc -> Int -> acc)
           -> acc
           -> Expr
           -> acc
foldlExpr' _ g !acc (C c)     = g acc c
foldlExpr' f g !acc (V a x e) = foldlExpr' f g (f acc a x) e

-- |Strict monadic fold.
foldExprM' :: Monad m => (acc -> Int -> Int -> m acc) -> (acc -> Int -> m acc) -> acc -> Expr -> m acc
foldExprM' _ g !acc (C c)     = g acc c
foldExprM' f g !acc (V a x e) = f acc a x >>= \acc' -> foldExprM' f g acc' e

infixl 5 .+
infixl 6 .*

-- |Addition of two expressions.
--
-- Time complexity is O(n + m) and the laws for additon holds.
--
-- prop> a .+ b == b .+ a
-- prop> a .+ (b .+ c) == (a .+ b) .+ c
-- prop> a .+ Zero == a
-- prop> Zero .+ a == a
-- prop> invariant (a .+ b)
-- prop> evalExpr id (a .+ b) == evalExpr id a + evalExpr id b
(.+) :: Expr -> Expr -> Expr
(.+) (C n1)       (C n2)       = C (n1 + n2)
(.+) (V a1 x1 e1) (C n2)       = V a1 x1 (e1 .+ C n2)
(.+) (C n1)       (V a2 x2 e2) = V a2 x2 (C n1 .+ e2)
(.+) (V a1 x1 e1) (V a2 x2 e2) = case compare x1 x2 of
  LT -> V a2 x2        $ V a1 x1 e1 .+ e2
  EQ -> f (a1 + a2) x1 $ e1 .+ e2
  GT -> V a1 x1        $ e1 .+ V a2 x2 e2

  where
    f 0 _ e = e
    f a x e = V a x e

-- |Multiply an expression with a constant.
--
-- Time complexity O(n).
--
-- prop> 0 .* e == Zero
-- prop> 1 .* e == e
-- prop> a .* Zero == Zero
-- prop> a .* Constant 1 == Constant a
-- prop> a .* (b .* e) == (a*b) .* e
-- prop> invariant (a .* e)
-- prop> evalExpr id (a .* e) == a * evalExpr id e
(.*) :: Int -> Expr -> Expr
n .* C m     = C (n*m)
n .* V a x e = f (n*a) x (n .* e)
  where
    f 0  _  e' = e'
    f a' x' e' = V a' x' e'

-- |Special case of '(.+)' where the added expression is a constant.
--
-- prop> invariant (addConstant e n)
-- prop> evalExpr id (e `addConstant` n) == evalExpr id e + n
addConstant :: Expr -> Int -> Expr
addConstant (C n) m     = C (n+m)
addConstant (V a x e) m = V a x (addConstant e m)

-- |Shift all variables in an expression.
--
-- Time complexity O(n)
--
-- prop> shiftExpr e 0 == e
-- prop> invariant (shiftExpr e n)
shiftExpr :: Expr -> Int -> Expr
shiftExpr (C n)      _ = C n
shiftExpr (V a x e) !s = V a (x+s) (shiftExpr e s)

-- |Evaluate an expression using a function for resolving variables.
--
-- Time complexity O(n) if f is constant time.
evalExpr :: (Int -> Int) -> Expr -> Int
evalExpr f = go 0
  where
    go !acc (C c)     = acc + c
    go !acc (V n v e) = go (acc + n * f v) e

-- |Set the value of an variable in an expression to the value of another
-- expression. That is, if the variable exists in the first expression, the
-- second expression is multiplied with the multiple of the variable and then
-- summed with the first expression. If the variable does not exist, the first
-- expression is returned unmodified.
--
-- >>> insertExpr (V 1 2 (V 1 1 (C 0))) (1, V 1 3 (C 1))
-- V 1 3 (V 1 2 (C 1))
--
-- prop> invariant (insertExpr e1 (x, e2))
insertExpr :: Expr -> (Int, Expr) -> Expr
insertExpr (C n)        _                    = C n
insertExpr (V a1 x1 e1) (x2, e2) | x1 == x2  = (a1 .* e2) .+ e1
                                 | otherwise = V a1 x1 Zero .+ insertExpr e1 (x2, e2)

-- |Special case of 'insertExpr' when the expression inserted is a constant.
--
-- Time complexity O(n).
--
-- prop> insertConst e (x, n) == insertExpr e (x, C n)
-- prop> invariant (insertConst e (x, n))
insertConst :: Expr -> (Int, Int) -> Expr
insertConst (C n)      _                  = C n
insertConst (V a x' e) (x, n) | x == x'   = e `addConstant` (a * n)
                              | otherwise = V a x' (insertConst e (x, n))

-- |Insert the expression from supplied map for each variable.
--
-- prop> insertExprs e mempty == e
-- prop> invariant (insertExprs e (M.fromList m))
insertExprs :: Expr -> M.IntMap Expr -> Expr
insertExprs = M.foldlWithKey' (\e1 x e2 -> insertExpr e1 (x, e2))

-- TODO: Improve by traversing both structures at the same time.

buildExpr :: (a -> a -> a) -> (Int -> Int -> a) -> (Int -> a) -> Expr -> a
buildExpr f g h = \case
  C n         -> h n
  V a x (C 0) -> g a x
  V a x e     -> g a x `f` buildExpr f g h e

{-# INLINE deconsExpr #-}
deconsExpr :: (Int -> Int -> Expr -> a) -> (Int -> a) -> Expr -> a
deconsExpr f g = \case
  C n     -> g n
  V a x e -> f a x e
