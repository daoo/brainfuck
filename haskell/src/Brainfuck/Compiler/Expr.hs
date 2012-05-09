module Brainfuck.Compiler.Expr where

import Test.QuickCheck

data Expr = Get Int
          | Const Int
          | Mult Expr Expr
          | Plus Expr Expr
  deriving (Show, Eq)

instance Arbitrary Expr where
  arbitrary = do
    i <- choose (-10, 100)
    d <- choose (-5, 10)
    e1 <- arbitrary
    e2 <- arbitrary
    frequency [ (3, return $ Const i)
              , (3, return $ Get d)
              , (1, return $ e1 `Plus` e2)
              , (1, return $ e1 `Mult` e2) ]

modifyPtr :: (Int -> Int) -> Expr -> Expr
modifyPtr _ (Const v)    = Const v
modifyPtr f (Get i)      = Get $ f i
modifyPtr f (Mult e1 e2) = Mult (modifyPtr f e1) (modifyPtr f e2)
modifyPtr f (Plus e1 e2) = Plus (modifyPtr f e1) (modifyPtr f e2)

cleanExpr :: Expr -> Expr
cleanExpr expr = case expr of
  Const 0 `Plus` e -> cleanExpr e
  Const 0 `Mult` _ -> Const 0
  Const 1 `Mult` e -> cleanExpr e

  e `Plus` Const 0 -> cleanExpr e
  _ `Mult` Const 0 -> Const 0
  e `Mult` Const 1 -> cleanExpr e

  Const v1 `Plus` Const v2 -> Const $ v1 + v2
  Const v1 `Mult` Const v2 -> Const $ v1 * v2

  Const v1 `Mult` (Const v2 `Plus` e) -> cleanExpr $ Const (v1 * v2) `Plus` (Const v1 `Mult` e)
  Const v1 `Mult` (e `Plus` Const v2) -> cleanExpr $ Const (v1 * v2) `Plus` (Const v1 `Mult` e)
  (Const v1 `Plus` e) `Mult` Const v2 -> cleanExpr $ (Const v2 `Mult` e) `Plus` Const (v1 * v2)
  (e `Plus` Const v1) `Mult` Const v2 -> cleanExpr $ (Const v2 `Mult` e) `Plus` Const (v1 * v2)

  Const v1 `Plus` (Const v2 `Plus` e) -> cleanExpr $ Const (v1 + v2) `Plus` e
  Const v1 `Plus` (e `Plus` Const v2) -> cleanExpr $ Const (v1 + v2) `Plus` e
  (e `Plus` Const v1) `Plus` Const v2 -> cleanExpr $ e `Plus` Const (v1 + v2)
  (Const v1 `Plus` e) `Plus` Const v2 -> cleanExpr $ e `Plus` Const (v1 + v2)

  e1 `Plus` e2 -> cleanExpr e1 `Plus` cleanExpr e2
  e1 `Mult` e2 -> cleanExpr e1 `Mult` cleanExpr e2

  e -> e

inline :: Int -> Expr -> Expr -> Expr
inline d ei (Get i) | d == i = ei
inline d ei (Mult e1 e2)     = inline d ei e1 `Mult` inline d ei e2
inline d ei (Plus e1 e2)     = inline d ei e1 `Plus` inline d ei e2
inline _ _ e                 = e
