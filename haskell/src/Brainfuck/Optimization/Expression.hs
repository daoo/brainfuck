{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Expression where

import Brainfuck.Data.Expr
import Brainfuck.Optimization.Rewriting

exprRules :: [Expr -> Rule Expr]
exprRules =
  [ evalAdd1
  , evalMul1
  , evalId
  , evalNegate
  , addZeroLeft
  , addZeroRight
  , mulOneLeft
  , mulOneRight
  , collapsNegate
  , swapConstGet
  , swapConstDown
  , rotateBinary
  , evalAdd2
  , evalMul2
  ]

evalAdd1 :: Expr -> Rule Expr
evalAdd1 (BinaryOp Add (Value (Const a)) (Value (Const b))) = return $ mkInt (a + b)
evalAdd1 e                                                  = fail (show e)

evalAdd2 :: Expr -> Rule Expr
evalAdd2 (BinaryOp Add (Value (Const a)) (BinaryOp Add b (Value (Const c)))) = return $ BinaryOp Add (mkInt (a + c)) b
evalAdd2 e                                                                   = fail (show e)

evalMul1 :: Expr -> Rule Expr
evalMul1 (BinaryOp Mul (Value (Const a)) (Value (Const b))) = return $ mkInt (a * b)
evalMul1 e                                                  = fail (show e)

evalMul2 :: Expr -> Rule Expr
evalMul2 (BinaryOp Mul (Value (Const a)) (BinaryOp Mul b (Value (Const c)))) = return $ BinaryOp Mul (mkInt (a * c)) b
evalMul2 e                                                                   = fail (show e)

evalNegate :: Expr -> Rule Expr
evalNegate (UnaryOp Negate (Value (Const a))) = return $ mkInt (-a)
evalNegate e                                  = fail (show e)

evalId :: Expr -> Rule Expr
evalId (UnaryOp Id e) = return e
evalId e              = fail (show e)

addZeroLeft :: Expr -> Rule Expr
addZeroLeft (BinaryOp Add (Value (Const 0)) b) = return b
addZeroLeft e                                  = fail (show e)

addZeroRight :: Expr -> Rule Expr
addZeroRight (BinaryOp Add a (Value (Const 0))) = return a
addZeroRight e                                  = fail (show e)

mulOneLeft :: Expr -> Rule Expr
mulOneLeft (BinaryOp Mul (Value (Const 1)) b) = return b
mulOneLeft e                                  = fail (show e)

mulOneRight :: Expr -> Rule Expr
mulOneRight (BinaryOp Mul a (Value (Const 1))) = return a
mulOneRight e                                  = fail (show e)

collapsNegate :: Expr -> Rule Expr
collapsNegate (UnaryOp Negate (UnaryOp Negate e)) = return e
collapsNegate e                                   = fail (show e)

swapConstGet :: Expr -> Rule Expr
swapConstGet (BinaryOp Add a@(Value (Const _)) b@(Value (Get _))) = return $ BinaryOp Add b a
swapConstGet (BinaryOp Mul a@(Value (Const _)) b@(Value (Get _))) = return $ BinaryOp Mul b a
swapConstGet e                                                    = fail (show e)

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

rotateBinary :: Expr -> Rule Expr
rotateBinary (BinaryOp Add (BinaryOp Add a b) c) = return $ BinaryOp Add a (BinaryOp Add b c)
rotateBinary (BinaryOp Mul (BinaryOp Mul a b) c) = return $ BinaryOp Mul a (BinaryOp Mul b c)
rotateBinary e                                   = fail (show e)
