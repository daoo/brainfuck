{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Expression where

import Brainfuck.Data.Expr
import Brainfuck.Optimization.Rule

exprRules :: [Expr -> Rule Expr]
exprRules =
  [ evalAdd1
  , evalAdd2
  , evalMul1
  , evalMul2
  , rotateAdd
  , addZeroLeft
  , addZeroRight
  , evalNegate
  , collapsNegate
  , evalId
  , moveConstRight
  , swapConstDown
  ]

evalAdd1 :: Expr -> Rule Expr
evalAdd1 (BinaryOp Add (Value (Const a)) (Value (Const b))) = return $ mkInt (a + b)
evalAdd1 e                                                  = fail (show e)

evalAdd2 :: Expr -> Rule Expr
evalAdd2 (BinaryOp Add (Value (Const a)) (BinaryOp Add (Value (Const b)) c)) = return $ BinaryOp Add (mkInt (a + b)) c
evalAdd2 e                                                                   = fail (show e)

evalMul1 :: Expr -> Rule Expr
evalMul1 (BinaryOp Mul (Value (Const a)) (Value (Const b))) = return $ mkInt (a * b)
evalMul1 e                                                  = fail (show e)

evalMul2 :: Expr -> Rule Expr
evalMul2 (BinaryOp Mul (Value (Const a)) (BinaryOp Mul (Value (Const b)) c)) = return $ BinaryOp Mul (mkInt (a * b)) c
evalMul2 e                                                                   = fail (show e)

addZeroLeft :: Expr -> Rule Expr
addZeroLeft (BinaryOp Add (Value (Const 0)) b) = return b
addZeroLeft e                                  = fail (show e)

addZeroRight :: Expr -> Rule Expr
addZeroRight (BinaryOp Add a (Value (Const 0))) = return a
addZeroRight e                                  = fail (show e)

evalNegate :: Expr -> Rule Expr
evalNegate (UnaryOp Negate (Value (Const a))) = return $ mkInt (-a)
evalNegate e                                  = fail (show e)

collapsNegate :: Expr -> Rule Expr
collapsNegate (UnaryOp Negate (UnaryOp Negate e)) = return e
collapsNegate e                                   = fail (show e)

evalId :: Expr -> Rule Expr
evalId (UnaryOp Id e) = return e
evalId e              = fail (show e)

moveConstRight :: Expr -> Rule Expr
moveConstRight (BinaryOp op a@(Value (Const _)) b@(Value (Get _))) = return $ BinaryOp op b a
moveConstRight e                                                   = fail (show e)

swapConstDown :: Expr -> Rule Expr
swapConstDown
  (BinaryOp Add
    a@(Value (Const _))
      (BinaryOp Add
        b@(Value (Get _))
        c)) = return $ BinaryOp Add b (BinaryOp Add a c)
swapConstDown
  (BinaryOp Mul
    a@(Value (Const _))
      (BinaryOp Mul
        b@(Value (Get _))
        c)) = return $ BinaryOp Mul b (BinaryOp Mul a c)
swapConstDown e = fail (show e)

rotateAdd :: Expr -> Rule Expr
rotateAdd (BinaryOp Add (BinaryOp Add a b) c) = return $ BinaryOp Add a (BinaryOp Add b c)
rotateAdd e                                   = fail (show e)

-- BinaryOp Add c (BinaryOp Mul (Value (Const a)) b) | b == c -> Just $ mkInt (a + 1) `mul` b

-- BinaryOp Add
--  (BinaryOp Mul (Value (Const a)) b)
--  (BinaryOp Mul (Value (Const c)) d) | b == d -> Just $ mkInt (a + c) `mul` b

{-sort :: Expr -> Maybe Expr
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
-}
