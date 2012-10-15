{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import Control.Monad
import Data.Maybe
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


  shrink (Add a b) = a : b : [Add a' b' | (a', b') <- zip (shrink a) (shrink b)]
  shrink (Mul a b) = a : b : [Mul a' b' | (a', b') <- zip (shrink a) (shrink b)]
  shrink (Const i) = map Const $ shrink i
  shrink (Get d)   = map Get $ shrink d

-- For easier testing
instance Num Expr where
  fromInteger = Const . fromInteger

  (+) = Add
  (*) = Mul

  abs    = undefined
  signum = undefined

unfold :: (a -> a -> a) -> (a -> a -> a) -> (Expr -> a) -> Expr -> a
unfold add mul f = \case
  Add a b -> unfold add mul f a `add` unfold add mul f b
  Mul a b -> unfold add mul f a `mul` unfold add mul f b
  e       -> f e

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
    p e = case foldr m (e, False) pipeline of
      (e', True) -> p e'
      (_, False) -> e

    pipeline = [mult, sort, listify, clean]

    m g (e, b) = let (e', b') = f g e in (e', b || b')

    f g e = case g e of
      Nothing -> (e, False)
      Just e' -> (e', True)

mult :: Expr -> Maybe Expr
mult = treeOptimizer (\case
  Add a b         | a == b -> Just $ Const 2 `Mul` a
  Add a (Add b c) | a == b -> Just $ Add (Const 2 `Mul` a) c

  Add c (Const a `Mul` b) | b == c -> Just $ Mul (Const a + 1) b

  Add (Const a `Mul` b) (Const c `Mul` d) | b == d -> Just $ Const (a + c) `Mul` b

  _ -> Nothing)

sort :: Expr -> Maybe Expr
sort = treeOptimizer (\case
  Add a@(Get _) b@(Const _)           -> Just $ Add b a
  Add a@(Get d1) b@(Get d2) | d1 > d2 -> Just $ Add b a

  Add a@(Get d1) (Add b@(Get d2) c) | d1 > d2 -> Just $ Add b (Add a c)

  Add a@(Get _) (Add b@(Const _) c) -> Just $ Add b (Add a c)

  _ -> Nothing)

listify :: Expr -> Maybe Expr
listify = treeOptimizer (\case
  Const a `Add` Const b -> Just $ Const (a + b)
  Const a `Mul` Const b -> Just $ Const (a * b)

  Get _   `Add` Get _ -> Nothing
  Const _ `Add` Get _ -> Nothing
  Get _   `Mul` Get _ -> Nothing
  Const _ `Mul` Get _ -> Nothing

  a@(Get _) `Add` b@(Const _) -> Just $ b `Add` a
  a@(Get _) `Mul` b@(Const _) -> Just $ b `Mul` a

  a `Add` b@(Const _) -> Just $ b `Add` a
  a `Add` b@(Get _)   -> Just $ b `Add` a
  a `Mul` b@(Const _) -> Just $ b `Mul` a
  a `Mul` b@(Get _)   -> Just $ b `Mul` a

  Add (Add a b) c -> Just $ Add a (Add b c)

  _ -> Nothing)

clean :: Expr -> Maybe Expr
clean = treeOptimizer (\case
  Const 0 `Add` b -> Just $ b
  Const 0 `Mul` _ -> Just $ Const 0
  Const 1 `Mul` b -> Just $ b
  a `Add` Const 0 -> Just $ a
  _ `Mul` Const 0 -> Just $ Const 0
  a `Mul` Const 1 -> Just $ a

  Const a `Add` Const b -> Just $ Const (a + b)
  Const a `Mul` Const b -> Just $ Const (a * b)

  Const a `Add` (Const b `Add` c) -> Just $ Const (a + b) `Add` c
  Const a `Mul` (Const b `Mul` c) -> Just $ Const (a * b) `Mul` c
  Const a `Mul` (Const b `Add` c) -> Just $ Const (a * b) `Add` (Const a `Mul` c)

  _ -> Nothing)

-- |Recursivley optimize an Expr tree using a node-level optimization function.
-- Uses depth first recursion. Get and Const results in Nothing. Add and Mul
-- are a little more complex:
--   For a node e@(BinOp a b) we first recurse to its children
--
--     a' = treeOptimizer f a
--     b' = treeOptimizer f b
--
--   Then there are two cases:
--
--     * Both are Nothing, no optimization happend, we return (f e)
--
--     * a' or b' is just, an optimization has occured and therefore the
--       entire treeOptimizer call have to return (Just someExpr). Thus
--       we must rebuild the node and return either the optimization or
--       a MORE optimized expression.
treeOptimizer :: (Expr -> Maybe Expr) -> Expr -> Maybe Expr
treeOptimizer f = \case
  Get _   -> Nothing
  Const _ -> Nothing
  Add a b -> opt Add a b
  Mul a b -> opt Mul a b

  where
    opt op a b = case (treeOptimizer f a, treeOptimizer f b) of
      (Nothing, Nothing) -> f $ op a b
      (Just a', Just b') -> mby $ op a' b'
      (Just a', Nothing) -> mby $ op a' b
      (Nothing, Just b') -> mby $ op a b'

    mby a = Just $ fromMaybe a (treeOptimizer f $ a)
