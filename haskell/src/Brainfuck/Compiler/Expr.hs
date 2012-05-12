module Brainfuck.Compiler.Expr where

import Brainfuck.Ext
import Test.QuickCheck

data Expr = Get Int
          | Const Int
          | Add Expr Expr
          | Mul Expr Expr
  deriving (Ord, Eq, Show)

instance Arbitrary Expr where
  arbitrary = do
    c <- choose (-10, 100)
    d <- choose (-5, 10)
    e1 <- arbitrary
    e2 <- arbitrary
    frequency [ (3, return $ Const c)
              , (2, return $ Get d)
              , (1, return $ Mul e1 e2)
              , (1, return $ Add e1 e2) ]

  shrink (Add e1 e2) = [e1, e2]
  shrink (Mul e1 e2) = [e1, e2]
  shrink _           = []

modifyPtr :: (Int -> Int) -> Expr -> Expr
modifyPtr _ (Const c)   = Const c
modifyPtr f (Get d)     = Get $ f d
modifyPtr f (Add e1 e2) = modifyPtr f e1 `Add` modifyPtr f e2
modifyPtr f (Mul e1 e2) = modifyPtr f e1 `Mul` modifyPtr f e2

-- |Create the (computionally) shortest expression that have the same results
optimizeExpr :: Expr -> Expr
optimizeExpr = finalize . whileModified (move . clean)
  where
    finalize e = case e of
      Mul (Const 2) e' -> Add e' e'

      Add e1 e2 -> finalize e1 `Add` finalize e2
      Mul e1 e2 -> finalize e1 `Mul` finalize e2

      _ -> e

    move e = case e of
      Add e1@(Get _) e2@(Const _) -> Add e2 e1

      Add (Add e1 e2) (Add e3 e4) -> move $ Add e1 (Add e2 (Add e3 e4))

      Add e1@(Get _) (Add e2@(Const _) e3) -> move $ Add e2 (Add e1 e3)

      Add (Add e1@(Const _) e2) e3 -> move $ Add e1 (Add e2 e3)

      Add e1 e2 -> move e1 `Add` move e2
      Mul e1 e2 -> move e1 `Mul` move e2

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
