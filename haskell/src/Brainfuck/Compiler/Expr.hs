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
  Plus (Const v1) (Const v2) -> Const $ v1 + v2
  Mult (Const v1) (Const v2) -> Const $ v1 * v2

  Plus e (Const 0) -> cleanExpr e
  Plus (Const 0) e -> cleanExpr e
  Mult _ (Const 0) -> Const 0
  Mult (Const 0) _ -> Const 0
  Mult e (Const 1) -> cleanExpr e
  Mult (Const 1) e -> cleanExpr e

  Plus e1 e2@(Const _) -> e2 `Plus` cleanExpr e1

  Mult (Const v1) (Plus (Const v2) e) -> Const (v1 * v2) `Plus` (Const v1 `Mult` e)
  
  Plus e1 (Plus e2 e3) -> Plus (Plus e1 e2) e3

  Plus e1 e2 -> cleanExpr e1 `Plus` cleanExpr e2
  Mult e1 e2 -> cleanExpr e1 `Mult` cleanExpr e2

  e -> e

inlineSet :: Int -> Expr -> Expr -> Expr
inlineSet d ei (Get i) | d == i = ei
inlineSet d ei (Mult e1 e2)     = inlineSet d ei e1 `Mult` inlineSet d ei e2
inlineSet d ei (Plus e1 e2)     = inlineSet d ei e1 `Plus` inlineSet d ei e2
inlineSet _ _ e                 = e

inlineAdd :: Int -> Expr -> Expr -> Expr
inlineAdd d ei (Get i) | d == i = ei `Plus` Get i
inlineAdd d ei (Mult e1 e2)     = inlineAdd d ei e1 `Mult` inlineAdd d ei e2
inlineAdd d ei (Plus e1 e2)     = inlineAdd d ei e1 `Plus` inlineAdd d ei e2
inlineAdd _ _ e                 = e
