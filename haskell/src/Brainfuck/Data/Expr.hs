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
unfold add mul f e = case e of
  Add e1 e2 -> unfold add mul f e1 `add` unfold add mul f e2
  Mul e1 e2 -> unfold add mul f e1 `mul` unfold add mul f e2
  _         -> f e

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
heigth e = case e of
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

    --pipeline = clean : intersperse clean [mult, sort, listify]

mult :: Expr -> (Expr, Bool)
mult = treeOptimizer opt
  where
    opt e = case e of
      Add e1 e2          | e1 == e2 -> (Mul (Const 2) e1, True)
      Add e1 (Add e2 e3) | e1 == e2 -> (Add (Mul (Const 2) e1) e3, True)

      Add (Mul (Const c) e1) (Add e2 e3) | e1 == e2 -> (Add (Mul (Const c + 1) e1) e3, True)

      _ -> (e, False)

sort :: Expr -> (Expr, Bool)
sort = treeOptimizer opt
  where
    opt e = case e of
      Add e1@(Get _) e2@(Const _)           -> (Add e2 e1, True)
      Add e1@(Get d1) e2@(Get d2) | d1 > d2 -> (Add e2 e1, True)

      Add e1@(Get d1) (Add e2@(Get d2) e3) | d1 > d2 -> (Add e2 (Add e1 e3), True)

      Add e1@(Get _) (Add e2@(Const _) e3) -> (Add e2 (Add e1 e3), True)

      _ -> (e, False)

listify :: Expr -> (Expr, Bool)
listify = treeOptimizer opt
  where
    opt e = case e of
      Const a `Add` Const b -> (Const (a + b), True)
      Get _ `Add` Get _     -> (e, False)
      Const _ `Add` Get _   -> (e, False)
      Get _ `Add` Const _   -> (e, False)

      Const a `Mul` Const b -> (Const (a * b), True)
      Get _ `Mul` Get _     -> (e, False)
      Const _ `Mul` Get _   -> (e, False)
      Get _ `Mul` Const _   -> (e, False)

      a `Add` b@(Const _) -> (b `Add` a, True)
      a `Add` b@(Get _)   -> (b `Add` a, True)
      a `Mul` b@(Const _) -> (b `Mul` a, True)
      a `Mul` b@(Get _)   -> (b `Mul` a, True)

      Add (Add e1 e2) e3 -> (Add e1 (Add e2 e3), True)
      Mul (Mul e1 e2) e3 -> (Mul e1 (Mul e2 e3), True)

      _ -> (e, False)

clean :: Expr -> (Expr, Bool)
clean = treeOptimizer opt
  where
    opt e = case e of
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

      Add (Mul (Const a) b) (Mul (Const c) d) | b == d -> (Const (a + c) `Mul` b, True)
      Add (Mul (Const a) b) c                 | b == c -> (Const (a + 1) `Mul` c, True)

      _ -> (e, False)

treeOptimizer :: (Expr -> (Expr, Bool)) -> Expr -> (Expr, Bool)
treeOptimizer f e = case f e of
  (Add a b, False) -> splitNode f Add a b
  (Mul a b, False) -> splitNode f Mul a b
  (e', False)      -> (e', False)
  (e', True)       -> (fst $ f e', True)

splitNode :: (a -> (b, Bool)) -> (b -> b -> c) -> a -> a -> (c, Bool)
splitNode f op a b = let (a', b1) = f a
                         (b', b2) = f b
                      in (a' `op` b', b1 || b2)
