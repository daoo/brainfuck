{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import Control.Applicative ((<$>),(<*>))
import Test.QuickCheck

data Value = Get !Int | Const !Int
  deriving (Ord, Eq, Show)

data BinaryOperator = Add | Mul
  deriving (Ord, Eq, Show)

data Expr = Return Value
          | OperateBinary BinaryOperator Expr Expr
  deriving (Ord, Eq, Show)

mkInt, mkGet :: Int -> Expr
mkInt = Return . Const
mkGet = Return . Get

isConst :: Expr -> Bool
isConst (Return (Const _)) = True
isConst _                  = False

add, mul :: Expr -> Expr -> Expr
add = OperateBinary Add
mul = OperateBinary Mul

instance Arbitrary Value where
  arbitrary = frequency
    [ (4, Const <$> arbitrary)
    , (1, Get <$> choose (-100, 100)) ]

  shrink = \case
    Const i -> map Const (shrink i)
    Get i   -> map Get (shrink i) ++ [Const 0]

instance Arbitrary BinaryOperator where
  arbitrary = oneof [return Add, return Mul]

  shrink = \case
    Add -> []
    Mul -> [Add]

instance Arbitrary Expr where
  arbitrary = sized $ \n -> expr n n
    where
      expr 0 _ = leaf
      expr n s = oneof [leaf, branch n s]

      branch n s = OperateBinary <$> arbitrary <*> (expr (n - 1) s) <*> (expr (n - 1) s)

      leaf = Return <$> arbitrary

  shrink = \case
    Return v             -> map Return $ shrink v
    OperateBinary op a b -> a : b : zipWith3 OperateBinary (cycle' (shrink op)) (shrink a) (shrink b)

    where
      cycle' [] = []
      cycle' xs = cycle xs

instance Num Expr where
  (+) = OperateBinary Add
  (*) = OperateBinary Mul

  negate = undefined

  abs    = undefined
  signum = undefined

  fromInteger = mkInt . fromInteger

unfold :: (BinaryOperator -> a -> a -> a) -> (Value -> a) -> Expr -> a
unfold binary value = \case
  Return v             -> value v
  OperateBinary op a b -> binary op (unfold' a) (unfold' b)
  where
    unfold' = unfold binary value

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 e = unfold OperateBinary f
  where
    f = \case
      Get d2 | d1 == d2 -> e
      v                 -> Return v

modifyValues :: (Value -> Expr) -> Expr -> Expr
modifyValues = unfold OperateBinary

eval :: (Int -> Int) -> Expr -> Int
eval f = unfold binary value
  where
    binary op a b = case op of
      Add -> a + b
      Mul -> a * b

    value = \case
      Const i -> i
      Get i   -> f i
