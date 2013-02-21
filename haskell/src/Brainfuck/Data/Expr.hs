{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import Control.Applicative ((<$>),(<*>))
import Test.QuickCheck

data Value = Get Int | Const Int
  deriving (Ord, Eq, Show)

data UnaryOp = Id | Negate
  deriving (Ord, Eq, Show)

data BinaryOp = Add | Mul
  deriving (Ord, Eq, Show)

data Expr = Value Value
          | UnaryOp UnaryOp Expr
          | BinaryOp BinaryOp Expr Expr
  deriving (Ord, Eq, Show)

mkInt, mkGet :: Int -> Expr
mkInt = Value . Const
mkGet = Value . Get

add, mul :: Expr -> Expr -> Expr
add = BinaryOp Add
mul = BinaryOp Mul

instance Arbitrary Value where
  arbitrary = frequency
    [ (4, Const <$> arbitrary)
    , (1, Get <$> choose (-100, 100)) ]

  shrink = \case
    Const i -> map Const (shrink i)
    Get i   -> Const 0 : map Get (shrink i)

instance Arbitrary UnaryOp where
  arbitrary = oneof [return Id, return Negate]

  shrink = \case
    Id     -> []
    Negate -> [Id]

instance Arbitrary BinaryOp where
  arbitrary = oneof [return Add, return Mul]

  shrink = \case
    Add -> []
    Mul -> [Add]

instance Arbitrary Expr where
  arbitrary = sized $ \n -> expr n n
    where
      expr 0 _ = leaf
      expr n s = oneof [leaf, branch n s]

      branch n s = frequency
        [ (1, UnaryOp <$> arbitrary <*> (expr (n - 1) s))
        , (4, BinaryOp <$> arbitrary <*> (expr (n - 1) s) <*> (expr (n - 1) s))
        ]

      leaf = Value <$> arbitrary

  shrink = \case
    Value v         -> map Value $ shrink v
    UnaryOp op a    -> a : zipWith UnaryOp (cycle (shrink op)) (shrink a)
    BinaryOp op a b -> a : b : zipWith3 BinaryOp (cycle (shrink op)) (shrink a) (shrink b)

instance Num Expr where
  (+) = BinaryOp Add
  (*) = BinaryOp Mul

  abs    = undefined
  signum = undefined

  fromInteger = Value . Const . fromInteger

unfold :: (UnaryOp -> a -> a) -> (BinaryOp -> a -> a -> a) -> (Value -> a) -> Expr -> a
unfold unary binary value = \case
  Value v         -> value v
  UnaryOp op a    -> unary op (unfold' a)
  BinaryOp op a b -> binary op (unfold' a) (unfold' b)
  where
    unfold' = unfold unary binary value

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 e = unfold UnaryOp BinaryOp f
  where
    f = \case
      Get d2 | d1 == d2 -> e
      v                 -> Value v

modifyValues :: (Value -> Expr) -> Expr -> Expr
modifyValues = unfold UnaryOp BinaryOp

eval :: (Int -> Int) -> Expr -> Int
eval f = unfold unary binary value
  where
    unary op a = case op of
      Id     -> a
      Negate -> -a

    binary op a b = case op of
      Add -> a + b
      Mul -> a * b

    value = \case
      Const i -> i
      Get i   -> f i

nodeCount :: Expr -> Int
nodeCount = \case
  Value _        -> 1
  UnaryOp _ a    -> 1 + nodeCount a
  BinaryOp _ a b -> 1 + nodeCount a + nodeCount b

heigth :: Expr -> Int
heigth = \case
  Value _        -> 1
  UnaryOp _ a    -> 1 + heigth a
  BinaryOp _ a b -> 1 + max (heigth a) (heigth b)
