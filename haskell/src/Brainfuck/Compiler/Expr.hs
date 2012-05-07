module Brainfuck.Compiler.Expr where

data Expr = Get Int
          | Const Int
          | Mult Expr Expr
          | Plus Expr Expr
  deriving (Show, Eq)

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
  Plus e1 e2       -> Plus (cleanExpr e1) (cleanExpr e2)
  Mult e1 e2       -> Mult (cleanExpr e1) (cleanExpr e2)

  e -> e
