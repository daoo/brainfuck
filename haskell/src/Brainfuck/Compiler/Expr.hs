module Brainfuck.Compiler.Expr where

import Test.QuickCheck

data Expr = Get Int
          | Const Int
          | Add Expr Expr
          | Mul Expr Expr
  deriving (Eq, Show)

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
modifyPtr f (Mul e1 e2) = modifyPtr f e1 `Mul` modifyPtr f e2
modifyPtr f (Add e1 e2) = modifyPtr f e1 `Add` modifyPtr f e2

optimizeExpr :: Expr -> Expr
optimizeExpr = opt
  where
    opt e = case e of
      Const _ -> e
      Get _   -> e

      Add (Const 0) e' -> opt e'
      Add e' (Const 0) -> opt e'
      Mul (Const 0) e' -> Const 0
      Mul e' (Const 0) -> Const 0
      Mul (Const 1) e' -> opt e'
      Mul e' (Const 1) -> opt e'

      Add (Const c1) (Const c2) -> Const $ c1 + c2
      Mul (Const c1) (Const c2) -> Const $ c1 * c2

      Add (Const c1) (Add (Const c2) e') -> opt $ Add (Const $ c1 + c2) e'

      Add e' (Const c)          -> opt $ Add (Const c) e'
      Add (Add (Const c) e1) e2 -> opt $ Add (Const c) (Add e1 e2)

      Add e1 e2 -> optIf Add e1 e2
      Mul e1 e2 -> optIf Mul e1 e2

    optIf f e1 e2 = case (opt e1, opt e2) of
      (e1', e2') | e1' /= e1 || e2' /= e2 -> opt $ e1' `f` e2'
                 | otherwise              -> e1' `f` e2'

inline :: Int -> Expr -> Expr -> Expr
inline d1 e (Get d2) | d1 == d2  = e
                     | otherwise = Get d2

inline _ _ (Const c)   = Const c
inline d e (Add e1 e2) = inline d e e1 `Add` inline d e e2
inline d e (Mul e1 e2) = inline d e e1 `Mul` inline d e e2
