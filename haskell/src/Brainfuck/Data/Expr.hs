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
    p e = if b1 || b2 then p e2 else e2
      where
        (e1, b1) = clean e
        (e2, b2) = listify e1

    --pipeline = clean : intersperse clean [mult, sort, listify]

    mult = \case
      Add e1 e2          | e1 == e2 -> mult $ Mul (Const 2) e1
      Add e1 (Add e2 e3) | e1 == e2 -> mult $ Add (Mul (Const 2) e1) e3

      Add (Mul (Const c) e1) (Add e2 e3) | e1 == e2 -> mult $ Add (Mul (Const c + 1) e1) e3

      Add e1 e2 -> mult e1 `Add` mult e2
      Mul e1 e2 -> mult e1 `Mul` mult e2

      e -> e

    sort = \case
      Add e1@(Get _) e2@(Const _)             -> Add e2 e1
      Add e1@(Get d1) e2@(Get d2) | d1 > d2   -> Add e2 e1
                                  | otherwise -> Add e1 e2

      Add e1@(Get d1) (Add e2@(Get d2) e3) | d1 > d2   -> sort $ Add e2 (Add e1 e3)
                                           | otherwise -> Add e1 $ sort $ Add e2 (sort e3)

      Add e1@(Get _) (Add e2@(Const _) e3) -> sort $ Add e2 (Add e1 e3)

      Add e1 e2 -> sort e1 `Add` sort e2
      Mul e1 e2 -> sort e1 `Mul` sort e2

      e -> e

listify :: Expr -> (Expr, Bool)
listify = \case
  Add (Add e1 e2) e3 -> (Add e1 (Add e2 e3), True)
  Mul (Mul e1 e2) e3 -> (Mul e1 (Mul e2 e3), True)

  Add e1 e2 -> loop2 listify Add e1 e2
  Mul e1 e2 -> loop2 listify Mul e1 e2

  e -> (e, False)

clean :: Expr -> (Expr, Bool)
clean = \case
  Const 0 `Add` e -> (e, True)
  Const 0 `Mul` _ -> (Const 0, True)
  Const 1 `Mul` e -> (e, True)
  e `Add` Const 0 -> (e, True)
  _ `Mul` Const 0 -> (Const 0, True)
  e `Mul` Const 1 -> (e, True)

  Const c1 `Add` Const c2 -> (Const (c1 + c2), True)
  Const c1 `Mul` Const c2 -> (Const (c1 * c2), True)

  Const c1 `Add` (Const c2 `Add` e) -> (Const (c1 + c2) `Add` e, True)
  Const c1 `Mul` (Const c2 `Mul` e) -> (Const (c1 * c2) `Mul` e, True)
  Const c1 `Mul` (Const c2 `Add` e) -> (Const (c1 * c2) `Add` (Const c1 `Mul` e), True)

  Add (Mul (Const c1) e1) (Mul (Const c2) e2) | e1 == e2 -> (Const (c1 + c2) `Mul` e1, True)
  Add (Mul (Const c1) e1) e2                  | e1 == e2 -> (Const (c1 + 1) `Mul` e2, True)

  Add e1 e2 -> loop2 clean Add e1 e2
  Mul e1 e2 -> loop2 clean Mul e1 e2

  e -> (e, False)

loop :: (a -> (a, Bool)) -> (a, Bool) -> (a, Bool)
loop f = until (not . snd) (f . fst)

loop2 :: (a -> (a, Bool)) -> (a -> a -> a) -> a -> a -> (a, Bool)
loop2 f op a b | b1 || b2  = f (a' `op` b')
                | otherwise = (a `op` b, False)
  where
    (a', b1) = f a
    (b', b2) = f b
