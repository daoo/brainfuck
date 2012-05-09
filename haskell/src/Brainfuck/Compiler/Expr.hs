module Brainfuck.Compiler.Expr where

import Data.List
import Test.QuickCheck

data Expr = Get Int
          | Const Int
          | Mult [Expr]
          | Add [Expr] 
  deriving (Show, Eq)

instance Arbitrary Expr where
  arbitrary = do
    c <- choose (-10, 100)
    d <- choose (-5, 10)
    e1 <- arbitrary
    e2 <- arbitrary
    frequency [ (3, return $ Const c)
              , (3, return $ Get d)
              , (1, return $ Mult [e1, e2])
              , (1, return $ Add [e1, e2]) ]

modifyPtr :: (Int -> Int) -> Expr -> Expr
modifyPtr _ (Const c) = Const c
modifyPtr f (Get d)   = Get $ f d
modifyPtr f (Mult es) = Mult $ map (modifyPtr f) es
modifyPtr f (Add es)  = Add $ map (modifyPtr f) es

cleanExpr :: Expr -> Expr
cleanExpr expr = case expr of
  Add [Const c] -> Const c
  Add exs       -> let (c, exs') = add $ map cleanExpr exs in Add $ Const c : exs'

  where
    add []              = (0, [])
    add (Const c : exs) = let (c', exs') = add exs in (c + c', exs')
    add (ex : exs)      = let (c', exs') = add exs in (c', ex : exs')

inline :: Int -> Expr -> Expr -> Expr
inline d1 e (Get d2) | d1 == d2  = e
                     | otherwise = Get d2
inline d e (Const c)             = Const c
inline d e (Mult exs)            = Mult $ map (inline d e) exs
inline d e (Add exs)             = Add $ map (inline d e) exs
