module Brainfuck.Compiler.Expr where

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
  Add [e] -> cleanExpr e
  Add exs -> case add 0 $ map cleanExpr exs of
    [e]   -> e
    exs'  -> Add $ mergeGets $ exs'

  Mult [e] -> cleanExpr e
  Mult exs -> case mult 1 $ map cleanExpr exs of
    [e]    -> e
    exs'   -> Mult exs'

  _ -> expr

  where
    add 0 []               = []
    add i []               = [Const i]
    add i (Const c : xs)   = add (i + c) xs
    add i (x@(Add _) : xs) = case cleanExpr x of
      Add ys -> add i (xs ++ ys)
      x'     -> add i (x' : xs)
    add i (x : xs) = x : add i xs

    mult 1 []                = []
    mult i []                = [Const i]
    mult i (Const c : xs)    = mult (i * c) xs
    mult i (x@(Mult _) : xs) = case cleanExpr x of
      Mult ys -> mult i (xs ++ ys)
      x'      -> mult i (x' : xs)
    mult i (x : xs)          = x : mult i xs

    mergeGets = helper []
      where
        helper m []           = finalize m
        helper m (Get d : xs) = helper (ins d m) xs
        helper m (x : xs)     = x : helper m xs

        finalize []            = []
        finalize ((d, 1) : xs) = Get d : finalize xs
        finalize ((d, i) : xs) = Mult [Get d, Const i] : finalize xs

        ins d []                         = [(d, 1)]
        ins d ((d', i) : ys) | d == d'   = (d', i + d) : ys
                             | otherwise = (d', i) : ins d ys

inline :: Int -> Expr -> Expr -> Expr
inline d1 e (Get d2) | d1 == d2  = e
                     | otherwise = Get d2
inline _ _ (Const c)             = Const c
inline d e (Mult xs)             = Mult $ map (inline d e) xs
inline d e (Add xs)              = Add $ map (inline d e) xs
