{-# LANGUAGE BangPatterns, GADTs, FlexibleInstances #-}
module Brainfuck.Data.Expr
  ( Expr(..)

  , econst
  , evar
  , isZero
  , isConst
  , isConst'
  , isVarConst

  , findVar
  , filterVars
  , foldVarsR
  , foldVarsL'
  , foldExprM'

  , (.+)
  , (.*)
  , shiftExpr

  , evalExpr

  , insertExpr
  , insertConst
  ) where

import Control.Arrow (first, second)
import Test.QuickCheck

-- |An expression is a sum of multiples of variables and an constant.
-- Represented as a sorted list with the constant stored in the terminator
-- which allows for linear time addition of two expressions.
data Expr where
  Var   :: !Int -> !Int -> Expr -> Expr
  Const :: !Int -> Expr

econst :: Int -> Expr
econst = Const

evar :: Int -> Expr
evar v = Var 1 v (Const 0)

{-# INLINE isZero #-}
isZero :: Expr -> Bool
isZero (Const 0) = True
isZero _         = False

{-# INLINE isConst #-}
isConst :: Expr -> Bool
isConst (Const _) = True
isConst _         = False

{-# INLINE isConst' #-}
isConst' :: Int -> Expr -> Bool
isConst' a (Const b) = a == b
isConst' _ _         = False

isVarConst :: Int -> Int -> Expr -> Bool
isVarConst v c (Var 1 v' (Const c')) = v == v' && c == c'
isVarConst _ _ _                     = False

instance Show Expr where
  show (Const c)   = show c
  show (Var n v e) = shows n $ showString "*" $ shows v $ showString " + " $ show e

instance Eq Expr where
  Const c1     == Const c2     = c1 == c2
  Var n1 d1 xs == Var n2 d2 ys = n1 == n2 && d1 == d2 && xs == ys
  _            == _            = False

instance Arbitrary Expr where
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

-- |Find a variable and return its multiple.
--
-- Ignores the constant.
findVar :: Int -> Expr -> Maybe Int
findVar _ (Const _)                 = Nothing
findVar v (Var n v' xs) | v == v'   = Just n
                        | otherwise = findVar v xs

-- |Filter the variables of an expression.
--
-- Leaves the constant unchanged.
filterVars :: ((Int, Int) -> Bool) -> Expr -> Expr
filterVars _ e@(Const _)              = e
filterVars f (Var n v xs) | f (n, v)  = Var n v (filterVars f xs)
                          | otherwise = filterVars f xs

-- |Map the variables and the constant of an expression.
--
-- The function @f@ is required to be monotonic as the resulting expression is
-- not resorted. Time complexity O(n).
mapExpr :: ((Int, Int) -> (Int, Int)) -> (Int -> Int) -> Expr -> Expr
mapExpr _ g (Const c)    = Const (g c)
mapExpr f g (Var n v xs) = uncurry Var (f (n, v)) $ mapExpr f g xs

-- |Right fold variables in an expression.
--
-- Ignores the constant.
foldVarsR :: (Int -> Int -> a -> a) -> a -> Expr -> a
foldVarsR _ acc (Const _)    = acc
foldVarsR f acc (Var n v xs) = f n v $ foldVarsR f acc xs

-- |Strict left fold variables in an expression.
--
-- Ignores the constant.
foldVarsL' :: (a -> Int -> Int -> a) -> a -> Expr -> a
foldVarsL' _ !acc (Const _)    = acc
foldVarsL' f !acc (Var n v xs) = foldVarsL' f (f acc n v) xs

-- |Strict fold with a monadic variable lookup function and a different
-- function for the constant.
foldExprM' :: Monad m => (acc -> Int -> Int -> m acc) -> (acc -> Int -> acc) -> acc -> Expr -> m acc
foldExprM' _ g !acc (Const c)    = return $ g acc c
foldExprM' f g !acc (Var n v xs) = f acc n v >>= \acc' -> foldExprM' f g acc' xs

infixl 5 .+
infixl 6 .*

-- |Addition of two expressions.
--
-- Time complexity is O(n + m) and the laws for additon holds:
--
-- prop> a .+ b = b .+ a
-- prop> a .+ (b .+ c) = (a .+ b) .+ c
-- prop> a .+ 0 = a
-- prop> 0 .+ a = a
(.+) :: Expr -> Expr -> Expr
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

-- |Multiply an expression with a constant.
--
-- Time complexity O(n).
(.*) :: Int -> Expr -> Expr
(.*) n = mapExpr (first (*n)) (*n)

-- |Shift all variables in an expression
-- Time complexity O(n)
shiftExpr :: Int -> Expr -> Expr
shiftExpr s = mapExpr (second (+s)) id

-- |Evaluate an expression using a function for resolving variables
-- Time complexity O(n) if f is constant time
evalExpr :: (Int -> Int) -> Expr -> Int
evalExpr f = go 0
  where
    go !acc (Const c)    = acc + c
    go !acc (Var n v xs) = go (acc + n * f v) xs

-- |Insert the value of a variable into an expression.
-- Time complexity O(n + m)
insertExpr :: Int -> Expr -> Expr -> Expr
insertExpr v a b = case findVar v b of
  Nothing -> b
  Just n  -> (n .* a) .+ filterVars ((/= v) . snd) b

-- |Special case of 'insertExpr' when the value is a constant
-- Time complexity O(n)
insertConst :: Int -> Int -> Expr -> Expr
insertConst v c = go
  where
    go (Const c')                = Const c'
    go (Var n v' xs) | v == v'   = mapExpr id (+ (n * c)) xs
                     | otherwise = Var n v' $ go xs
