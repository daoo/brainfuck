{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import Control.Monad
import Test.QuickCheck

data Expr = Get Int
          | Const Int
          | Add Expr Expr
          | Mul Expr Expr
  deriving (Ord, Eq, Show)

instance Arbitrary Expr where
  arbitrary = expr
    where
      expr = sized $ \n -> expr' n n

      expr' 0 s = leaf s
      expr' n s = oneof [leaf s, branch n s]

      branch n s = oneof [ liftM2 Add (expr' (n - 1) s) (expr' (n - 1) s)
                         , liftM2 Mul (expr' (n - 1) s) (expr' (n - 1) s) ]
      leaf s = oneof [ liftM Const $ choose (-s, s)
                     , liftM Get $ choose (-s `div` 10, s `div` 5) ]


  shrink (Add e1 e2) = e1 : e2 : [Add e1' e2' | (e1', e2') <- zip (shrink e1) (shrink e2)]
  shrink (Mul e1 e2) = e1 : e2 : [Mul e1' e2' | (e1', e2') <- zip (shrink e1) (shrink e2)]
  shrink (Const i)   = map Const $ shrink i
  shrink (Get d)     = map Get $ shrink d

-- For easier testing
instance Num Expr where
  fromInteger = Const . fromInteger

  (+) = Add
  (*) = Mul

  abs    = undefined
  signum = undefined

unfold :: (a -> a -> a) -> (a -> a -> a) -> (Expr -> a) -> Expr -> a
unfold add mul f = \case
  Add e1 e2 -> unfold add mul f e1 `add` unfold add mul f e2
  Mul e1 e2 -> unfold add mul f e1 `mul` unfold add mul f e2
  e         -> f e

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 e = unfold Add Mul f
  where
    f (Get d2) | d1 == d2 = e
    f e'                  = e'

modifyLeafs :: (Expr -> Expr) -> Expr -> Expr
modifyLeafs = unfold Add Mul

eval :: (Int -> Int) -> Expr -> Int
eval = unfold (+) (*) . g
  where
    g _ (Const c) = c
    g f (Get d)   = f d
    g _ _         = error "unfold Expr error"

heigth :: Expr -> Int
heigth = \case
  Add a b -> 1 + max (heigth a) (heigth b)
  Mul a b -> 1 + max (heigth a) (heigth b)
  _       -> 1

-- |Create the (computionally) shortest expression that have the same results
optimizeExpr :: Expr -> Expr
optimizeExpr = p
  where
    p e = if b1 || b2 || b3 || b4 then p e4 else e4
      where
        (e1, b1) = clean e
        (e2, b2) = listify e1
        (e3, b3) = sort e2
        (e4, b4) = mult e3

mult :: Expr -> (Expr, Bool)
mult = treeOptimizer (\case
  Add a b         | a == b -> (Const 2 `Mul` a, True)
  Add a (Add b c) | a == b -> (Add (Const 2 `Mul` a) c, True)

  Add c (Const a `Mul` b) | b == c -> (Mul (Const a + 1) b, True)

  Add (Const a `Mul` b) (Const c `Mul` d) | b == d -> (Const (a + c) `Mul` b, True)

  e -> (e, False))

sort :: Expr -> (Expr, Bool)
sort = treeOptimizer (\case
  Add e1@(Get _) e2@(Const _)           -> (Add e2 e1, True)
  Add e1@(Get d1) e2@(Get d2) | d1 > d2 -> (Add e2 e1, True)

  Add e1@(Get d1) (Add e2@(Get d2) e3) | d1 > d2 -> (Add e2 (Add e1 e3), True)

  Add e1@(Get _) (Add e2@(Const _) e3) -> (Add e2 (Add e1 e3), True)

  e -> (e, False))

listify :: Expr -> (Expr, Bool)
listify = treeOptimizer (\case
  Const a `Add` Const b -> (Const (a + b), True)
  Const a `Mul` Const b -> (Const (a * b), True)

  e@(Get _   `Add` Get _) -> (e, False)
  e@(Const _ `Add` Get _) -> (e, False)
  e@(Get _   `Mul` Get _) -> (e, False)
  e@(Const _ `Mul` Get _) -> (e, False)

  a@(Get _) `Add` b@(Const _) -> (b `Add` a, True)
  a@(Get _) `Mul` b@(Const _) -> (b `Mul` a, True)

  a `Add` b@(Const _) -> (b `Add` a, True)
  a `Add` b@(Get _)   -> (b `Add` a, True)
  a `Mul` b@(Const _) -> (b `Mul` a, True)
  a `Mul` b@(Get _)   -> (b `Mul` a, True)

  Add (Add a b) c -> (Add a (Add b c), True)

  e -> (e, False))

clean :: Expr -> (Expr, Bool)
clean = treeOptimizer (\case
  Const 0 `Add` b -> (b, True)
  Const 0 `Mul` _ -> (Const 0, True)
  Const 1 `Mul` b -> (b, True)
  a `Add` Const 0 -> (a, True)
  _ `Mul` Const 0 -> (Const 0, True)
  a `Mul` Const 1 -> (a, True)

  Const a `Add` Const b -> (Const (a + b), True)
  Const a `Mul` Const b -> (Const (a * b), True)

  Const a `Add` (Const b `Add` c) -> (Const (a + b) `Add` c, True)
  Const a `Mul` (Const b `Mul` c) -> (Const (a * b) `Mul` c, True)
  Const a `Mul` (Const b `Add` c) -> (Const (a * b) `Add` (Const a `Mul` c), True)

  e -> (e, False))

treeOptimizer :: (Expr -> (Expr, Bool)) -> Expr -> (Expr, Bool)
treeOptimizer f e = case f e of
  (Add a b, False) -> splitNode (treeOptimizer f) Add a b
  (Mul a b, False) -> splitNode (treeOptimizer f) Mul a b
  (e', False)      -> (e', False)
  (e', True)       -> (fst $ f e', True)

splitNode :: (a -> (b, Bool)) -> (b -> b -> c) -> a -> a -> (c, Bool)
splitNode f op a b = let (a', b1) = f a
                         (b', b2) = f b
                      in (a' `op` b', b1 || b2)
