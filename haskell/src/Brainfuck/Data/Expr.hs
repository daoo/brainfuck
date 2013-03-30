{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Brainfuck.Data.Expr where

import Control.Applicative hiding (Const)
import Control.Monad
import Test.QuickCheck

-- Use a newtype for keeping my sanity
newtype Mult = Mult { mkMult :: Int }
  deriving (Show, Eq, Ord, Num, Arbitrary)

-- Sorted list of multiples of variables terminated by a constant
data Expr = Const {-# UNPACK #-} !Int
          | Var {-# UNPACK #-} !Mult {-# UNPACK #-} !Int Expr
  deriving Show

instance Arbitrary Expr where
  arbitrary = frequency
    [ (1, Var <$> arbitrary <*> arbitrary <*> arbitrary)
    , (5, Const <$> arbitrary)
    ]

variable :: Int -> Expr
variable d = Var (Mult 1) d (Const 0)

foldExpr :: (b -> b -> b) -> (Int -> b) -> (Mult -> Int -> b) -> b -> Expr -> b
foldExpr f g h = go
  where
    go acc = \case
      Const c    -> f acc (g c)
      Var n d xs -> go (f acc (h n d)) xs

foldExpr1 :: (Int -> b) -> (Mult -> Int -> b -> b) -> Expr -> b
foldExpr1 f g = go
  where
    go = \case
      Const c    -> f c
      Var n d xs -> g n d (go xs)

findMult :: Int -> Expr -> Maybe Mult
findMult d = foldExpr1 (const Nothing) (\m d' x -> if d == d' then Just m `mplus` x else x)

filterVars :: (Int -> Bool) -> Expr -> Expr
filterVars f = foldExpr1 Const (\m d x -> if f d then Var m d x else x)

add :: Expr -> Expr -> Expr
add (Const c1)         (Const c2)     = Const $ c1 + c2
add x@(Const _)        (Var m2 d2 ys) = Var m2 d2 (add x ys)
add (Var m1 d1 xs)   y@(Const _)      = Var m1 d1 (add xs y)
add x@(Var m1 d1 xs) y@(Var m2 d2 ys) = case compare d1 d2 of
  LT -> Var m1 d1 (add xs y)
  EQ -> Var (m1 + m2) d1 (add xs ys)
  GT -> Var m2 d2 (add x ys)

eval :: (Int -> Int) -> Expr -> Int
eval f = foldExpr (+) id (\(Mult n) d -> n * f d) 0

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d a b = case findMult d b of
  Nothing       -> b
  Just (Mult n) -> foldExpr1 (Const . (*n)) (f n) a `add` filterVars (==d) b

  where
    f n (Mult n') d' x = Var (Mult $ n * n') d' x
