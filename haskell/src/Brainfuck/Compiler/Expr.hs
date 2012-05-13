module Brainfuck.Compiler.Expr where

import Brainfuck.Ext
import Control.Monad
import Data.List (intersperse)
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
      leaf s     = oneof [ liftM Const $ choose (-s, s)
                         , liftM Get $ choose (-s `div` 10, s `div` 5) ]


  shrink (Add e1 e2) = [e1, e2] ++ shrink e1 ++ shrink e2
  shrink (Mul e1 e2) = [e1, e2] ++ shrink e1 ++ shrink e2
  shrink _           = []

-- For easier testing
instance Num Expr where
  fromInteger = Const . fromInteger

  (+) = Add
  (*) = Mul

  abs    = undefined
  signum = undefined

modifyPtr :: (Int -> Int) -> Expr -> Expr
modifyPtr _ (Const c)   = Const c
modifyPtr f (Get d)     = Get $ f d
modifyPtr f (Add e1 e2) = modifyPtr f e1 `Add` modifyPtr f e2
modifyPtr f (Mul e1 e2) = modifyPtr f e1 `Mul` modifyPtr f e2

eval :: (Int -> Int) -> Expr -> Int
eval _ (Const c)   = c
eval f (Get d)     = f d
eval f (Add e1 e2) = eval f e1 + eval f e2
eval f (Mul e1 e2) = eval f e1 * eval f e2

-- |Create the (computionally) shortest expression that have the same results
optimizeExpr :: Expr -> Expr
optimizeExpr = finalize . whileModified (pipe pipeline)
  where
    pipeline = clean : intersperse clean [mult, sort, listify]

    finalize e = case e of
      Mul (Const 2) e'@(Get _) -> Add e' e'

      Add e1 e2 -> finalize e1 `Add` finalize e2
      Mul e1 e2 -> finalize e1 `Mul` finalize e2

      _ -> e

    mult e = case e of
      Add e1 e2          | e1 == e2 -> mult $ Mul (Const 2) e1
      Add e1 (Add e2 e3) | e1 == e2 -> mult $ Add (Mul (Const 2) e1) e3

      Add e1 (Add (Mul (Const i) e2) e3) | e1 == e2 -> Add (Mul (Const i + 1) e2) e3

      Add e1 e2 -> mult e1 `Add` mult e2
      Mul e1 e2 -> mult e1 `Mul` mult e2

      _ -> e

    listify e = case e of
      Add (Add e1 e2) e3 -> listify $ Add e1 (Add e2 e3)
      Mul (Mul e1 e2) e3 -> listify $ Mul e1 (Mul e2 e3)

      Add e1 e2 -> Add (listify e1) (listify e2)
      Mul e1 e2 -> Mul (listify e1) (listify e2)

      _ -> e

    sort e = case e of
      Add e1@(Get _) e2@(Const _)             -> Add e2 e1
      Add e1@(Get d1) e2@(Get d2) | d1 > d2   -> Add e2 e1
                                  | otherwise -> Add e1 e2

      Add e1@(Get d1) (Add e2@(Get d2) e3) | d1 > d2   -> sort $ Add e2 (Add e1 e3)
                                           | otherwise -> Add e1 $ sort $ Add e2 (sort e3)

      Add e1@(Get _) (Add e2@(Const _) e3) -> sort $ Add e2 (Add e1 e3)

      Add e1 e2 -> sort e1 `Add` sort e2
      Mul e1 e2 -> sort e1 `Mul` sort e2

      _ -> e

    clean e = case e of
      Add (Const 0) e' -> clean e'
      Add e' (Const 0) -> clean e'
      Mul (Const 0) _  -> Const 0
      Mul _ (Const 0)  -> Const 0
      Mul (Const 1) e' -> clean e'
      Mul e' (Const 1) -> clean e'

      Add (Const c1) (Const c2) -> Const $ c1 + c2
      Mul (Const c1) (Const c2) -> Const $ c1 * c2

      Add (Const c1) (Add (Const c2) e') -> clean $ Add (Const $ c1 + c2) e'
      Mul (Const c1) (Mul (Const c2) e') -> clean $ Mul (Const $ c1 * c2) e'
      Mul (Const c1) (Add (Const c2) e') -> clean $ Add (Const $ c1 * c2) (Mul (Const c1) e')

      Add (Mul (Const c1) e1) (Mul (Const c2) e2) | e1 == e2 -> clean $ Mul (Const $ c1 + c2) e1
      Add (Mul (Const c1) e1) e2                  | e1 == e2 -> clean $ Mul (Const $ c1 + 1) e2

      Add e1 e2 -> clean e1 `Add` clean e2
      Mul e1 e2 -> clean e1 `Mul` clean e2

      _ -> e

inline :: Int -> Expr -> Expr -> Expr
inline d1 e (Get d2) | d1 == d2  = e
                     | otherwise = Get d2

inline _ _ (Const c)   = Const c
inline d e (Add e1 e2) = inline d e e1 `Add` inline d e e2
inline d e (Mul e1 e2) = inline d e e1 `Mul` inline d e e2
