{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving #-}
module Brainfuck.Data.Expr where

import Brainfuck.Utility
import Test.QuickCheck
import qualified Data.List as List

-- Use a newtype for keeping my sanity
newtype Mult = Mult { mkMult :: Int }
  deriving (Eq, Ord, Num, Arbitrary)

newtype Var = Var { mkVar :: Int }
  deriving (Eq, Ord, Num, Arbitrary)

instance Show Mult where
  showsPrec _ (Mult n) = showString "Mult " . shows n

instance Show Var where
  showsPrec _ (Var d) = showString "Var " . shows d

type Variable = (Mult, Var)

-- |An expression is a constant and a sum of multiples of variables
data Expr = Expr { econst :: {-# UNPACK #-} !Int, evars :: [(Mult, Var)] }

instance Show Expr where
  showsPrec _ (Expr c v) = shows c . showString " + " . go v
    where
      go = \case
        []                   -> id
        (Mult n, Var d) : xs -> shows n . showString "#" . shows d . go xs

instance Arbitrary Expr where
  arbitrary = undefined -- TODO: Must be sorted

constant :: Int -> Expr
constant = (`Expr` [])

variable :: Int -> Expr
variable d = Expr 0 [(Mult 1, Var d)]

variable' :: Int -> Int -> Expr
variable' n d = Expr 0 [(Mult n, Var d)]

foldExpr :: (Int -> Variable -> Int) -> Expr -> Int
foldExpr f (Expr c v) = foldl f c v

findExpr :: Int -> Expr -> Maybe Variable
findExpr d = List.find ((==d) . mkVar . snd) . evars

filterExpr :: (Variable -> Bool) -> Expr -> Expr
filterExpr f (Expr c v) = Expr c (List.filter f v)

add :: Expr -> Expr -> Expr
add (Expr c1 v1) (Expr c2 v2) = Expr (c1 + c2) (merge v1 v2)
  where
    merge []              ys              = ys
    merge xs              []              = xs
    merge (x@(m1, d1):xs) (y@(m2, d2):ys) = case compare d1 d2 of
      LT -> x             : merge xs (y:ys)
      EQ -> (m1 + m2, d1) : merge xs ys
      GT -> y             : merge (x:xs) ys

eval :: (Int -> Int) -> Expr -> Int
eval f = foldExpr (\acc (Mult n, Var d) -> acc + n * f d)

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d (Expr c v) b = case findExpr d b of
  Nothing              -> b
  Just (m@(Mult n), _) ->
    add
      (Expr (c * n) (map (mapFst (*m)) v))
      (filterExpr ((/= d) . mkVar . snd) b)
