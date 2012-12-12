{-# LANGUAGE LambdaCase #-}
module Brainfuck.Data.Expr where

import Control.Applicative ((<$>),(<*>),(<|>))
import Data.Maybe
import Test.QuickCheck

data Value = Get Int | Const Int
  deriving (Ord, Eq, Show)

data UnaryOp = Id | Negate
  deriving (Ord, Eq, Show)

data BinaryOp = Add | Mul
  deriving (Ord, Eq, Show)

data Expr = Value Value
          | UnaryOp UnaryOp Expr
          | BinaryOp BinaryOp Expr Expr
  deriving (Ord, Eq, Show)

mkInt, mkGet :: Int -> Expr
mkInt = Value . Const
mkGet = Value . Get

add, mul :: Expr -> Expr -> Expr
add = BinaryOp Add
mul = BinaryOp Mul

instance Arbitrary Value where
  arbitrary = frequency
    [ (4, Const <$> arbitrary)
    , (1, Get <$> choose (-100, 100)) ]

  shrink = \case
    Const i -> map Const (shrink i)
    Get i   -> Const 0 : map Get (shrink i)

instance Arbitrary UnaryOp where
  arbitrary = oneof [return Id, return Negate]

  shrink = \case
    Id     -> []
    Negate -> [Id]

instance Arbitrary BinaryOp where
  arbitrary = oneof [return Add, return Mul]

  shrink = \case
    Add -> []
    Mul -> [Add]

instance Arbitrary Expr where
  arbitrary = sized $ \n -> expr n n
    where
      expr 0 _ = leaf
      expr n s = oneof [leaf, branch n s]

      branch n s = frequency
        [ (1, UnaryOp <$> arbitrary <*> (expr (n - 1) s))
        , (4, BinaryOp <$> arbitrary <*> (expr (n - 1) s) <*> (expr (n - 1) s))
        ]

      leaf = Value <$> arbitrary

  shrink = \case
    Value v         -> map Value $ shrink v
    UnaryOp op a    -> a : zipWith UnaryOp (cycle (shrink op)) (shrink a)
    BinaryOp op a b -> a : b : zipWith3 BinaryOp (cycle (shrink op)) (shrink a) (shrink b)

unfold :: (UnaryOp -> a -> a) -> (BinaryOp -> a -> a -> a) -> (Value -> a) -> Expr -> a
unfold unary binary value = \case
  Value v         -> value v
  UnaryOp op a    -> unary op (unfold' a)
  BinaryOp op a b -> binary op (unfold' a) (unfold' b)
  where
    unfold' = unfold unary binary value

inlineExpr :: Int -> Expr -> Expr -> Expr
inlineExpr d1 e = unfold UnaryOp BinaryOp f
  where
    f = \case
      Get d2 | d1 == d2 -> e
      v                 -> Value v

modifyValues :: (Value -> Expr) -> Expr -> Expr
modifyValues = unfold UnaryOp BinaryOp

eval :: (Int -> Int) -> Expr -> Int
eval f = unfold unary binary value
  where
    unary op a = case op of
      Id     -> a
      Negate -> -a

    binary op a b = case op of
      Add -> a + b
      Mul -> a * b

    value = \case
      Const i -> i
      Get i   -> f i

nodeCount :: Expr -> Int
nodeCount = \case
  Value _        -> 1
  UnaryOp _ a    -> 1 + nodeCount a
  BinaryOp _ a b -> 1 + nodeCount a + nodeCount b

heigth :: Expr -> Int
heigth = \case
  Value _        -> 1
  UnaryOp _ a    -> 1 + heigth a
  BinaryOp _ a b -> 1 + max (heigth a) (heigth b)

-- |Create the (computionally) shortest expression that have the same results
optimizeExpr :: Expr -> Expr
optimizeExpr = go
  where
    go e = maybe e go $ foldl (func e) Nothing pipeline

    pipeline = [mult, sort, listify, clean]

    func e acc f = case acc of
      Nothing -> treeOptimizer f e
      Just e' -> Just $ fromMaybe e' (treeOptimizer f e')

mult :: Expr -> Maybe Expr
mult = \case
  BinaryOp Add a b                  | a == b -> Just $ (Value $ Const 2) `mul` a
  BinaryOp Add a (BinaryOp Add b c) | a == b -> Just $ (mkInt 2 `mul` a) `add` c

  BinaryOp Add c (BinaryOp Mul (Value (Const a)) b) | b == c -> Just $ mkInt (a + 1) `mul` b

  BinaryOp Add
    (BinaryOp Mul (Value (Const a)) b)
    (BinaryOp Mul (Value (Const c)) d) | b == d -> Just $ mkInt (a + c) `mul` b

  _ -> Nothing

sort :: Expr -> Maybe Expr
sort = \case
  BinaryOp Add a b -> case (a, b) of
    (Value a', Value b') -> case (a', b') of
      (Get _, Const _)           -> Just $ b `add` a
      (Get d1, Get d2) | d1 > d2 -> Just $ b `add` a
      _                          -> Nothing

    (Value (Get i1), BinaryOp Add c@(Value (Get i2)) d) | i1 > i2 -> Just $ c `add` (a `add` d)
    (Value (Get _), BinaryOp Add c@(Value (Const _)) d)           -> Just $ c `add` (a `add` d)

    _ -> Nothing

  _ -> Nothing

listify :: Expr -> Maybe Expr
listify = \case
  BinaryOp Add a@(Value (Get _)) b@(Value (Const _)) -> Just $ b `add` a
  BinaryOp Mul a@(Value (Get _)) b@(Value (Const _)) -> Just $ b `mul` a

  BinaryOp Add (BinaryOp Add a b) c -> Just $ a `add` (b `add` c)

  _ -> Nothing

clean :: Expr -> Maybe Expr
clean = \case
  UnaryOp Id a                      -> Just a
  UnaryOp Negate (UnaryOp Negate a) -> Just a
  UnaryOp Negate (Value (Const a))  -> Just $ mkInt (-a)

  BinaryOp Add (Value (Const 0)) b -> Just b
  BinaryOp Add a (Value (Const 0)) -> Just a

  BinaryOp Add (Value (Const a')) (Value (Const b')) -> Just $ mkInt (a' + b')

  BinaryOp Mul (Value (Const 0)) _ -> Just $ mkInt 0
  BinaryOp Mul _ (Value (Const 0)) -> Just $ mkInt 0

  BinaryOp Mul (Value (Const 1)) b -> Just b
  BinaryOp Mul a (Value (Const 1)) -> Just a

  BinaryOp Mul (Value (Const a')) (Value (Const b')) -> Just $ mkInt (a' * b')

  BinaryOp op1 (Value (Const c1)) (BinaryOp op2 (Value (Const c2)) e) -> case (op1, op2) of
    (Add, Add) -> Just $ mkInt (c1 + c2) `add` e
    (Add, Mul) -> Nothing
    (Mul, Add) -> Just $ mkInt (c1 * c2) `add` (mkInt c1 `mul` e)
    (Mul, Mul) -> Just $ mkInt (c1 * c2) `mul` e

  _ -> Nothing

-- |Recursivley optimize an Expr tree using a node-level optimization function.
-- Uses depth first recursion. Get and Const results in Nothing. Add and Mul
-- are a little more complex:
--   For a node e@(BinaryOp a b) we first recurse to its children
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
treeOptimizer f = go
  where
    go = \case
      Value _ -> Nothing

      UnaryOp op a -> case go a of
        Nothing -> f $ UnaryOp op a
        Just a' -> let e' = UnaryOp op a'
                    in go e' <|> Just e'

      BinaryOp op a b -> case (go a, go b) of
        (Nothing, Nothing) -> f $ BinaryOp op a b
        (a', b')           -> let e' = BinaryOp op (fromMaybe a a') (fromMaybe b b')
                              in go e' <|> Just e'
