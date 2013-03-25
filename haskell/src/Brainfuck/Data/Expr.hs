{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import Control.Applicative ((<$>),(<*>))
import Test.QuickCheck

data Expr = Const !Int
          | Var !Int
          | Add Expr Expr
          | Mul !Int Expr
  deriving (Ord, Eq, Show)

isConst :: Expr -> Bool
isConst (Const _) = True
isConst _         = False

instance Arbitrary Expr where
  arbitrary = sized $ \n -> expr n n
    where
      expr 0 _ = leaf
      expr n s = oneof [leaf, branch n s]

      branch n s = frequency
        [ (4, Add <$> (expr (n - 1) s) <*> (expr (n - 1) s))
        , (1, Mul <$> arbitrary <*> (expr (n - 1) s))
        ]

      leaf = frequency
        [ (4, Const <$> arbitrary)
        , (1, Var <$> choose (-100, 100)) ]

  shrink = \case
    Const i -> map Const (shrink i)
    Var i   -> map Var (shrink i) ++ [Const 0]

    Add a b -> a : b : zipWith Add (shrink a) (shrink b)
    Mul _ b -> shrink b

unfold :: (a -> a -> a) -> (Int -> a -> a) -> (Int -> a) -> (Int -> a) -> Expr -> a
unfold add mul cst var = \case
  Const a -> cst a
  Var a   -> var a
  Add a b -> add (unfold' a) (unfold' b)
  Mul a b -> mul a (unfold' b)
  where
    unfold' = unfold add mul cst var

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 e = unfold Add Mul Const (\d2 -> if d1 == d2 then e else Var d2)

modifyVars :: (Int -> Expr) -> Expr -> Expr
modifyVars = unfold Add Mul Const

eval :: (Int -> Int) -> Expr -> Int
eval f = unfold (+) (*) id f
