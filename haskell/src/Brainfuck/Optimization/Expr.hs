{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Expr where

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
evalAdd1 (OperateBinary Add (Value (Const a)) (Value (Const b))) = return $ mkInt (a + b)
evalAdd1 e                                                       = fail (show e)

evalAdd2 :: Expr -> Rule Expr
evalAdd2 (OperateBinary Add (Value (Const a)) (OperateBinary Add b (Value (Const c)))) = return $ OperateBinary Add (mkInt (a + c)) b
evalAdd2 e                                                                             = fail (show e)

evalMul1 :: Expr -> Rule Expr
evalMul1 (OperateBinary Mul (Value (Const a)) (Value (Const b))) = return $ mkInt (a * b)
evalMul1 e                                                       = fail (show e)

evalMul2 :: Expr -> Rule Expr
evalMul2 (OperateBinary Mul (Value (Const a)) (OperateBinary Mul b (Value (Const c)))) = return $ OperateBinary Mul (mkInt (a * c)) b
evalMul2 e                                                                             = fail (show e)

evalNegate :: Expr -> Rule Expr
evalNegate (OperateUnary Negate (Value (Const a))) = return $ mkInt (-a)
evalNegate e                                       = fail (show e)

evalId :: Expr -> Rule Expr
evalId (OperateUnary Id e) = return e
evalId e                   = fail (show e)

addZeroLeft :: Expr -> Rule Expr
addZeroLeft (OperateBinary Add (Value (Const 0)) b) = return b
addZeroLeft e                                       = fail (show e)

addZeroRight :: Expr -> Rule Expr
addZeroRight (OperateBinary Add a (Value (Const 0))) = return a
addZeroRight e                                       = fail (show e)

mulOneLeft :: Expr -> Rule Expr
mulOneLeft (OperateBinary Mul (Value (Const 1)) b) = return b
mulOneLeft e                                       = fail (show e)

mulOneRight :: Expr -> Rule Expr
mulOneRight (OperateBinary Mul a (Value (Const 1))) = return a
mulOneRight e                                       = fail (show e)

collapsNegate :: Expr -> Rule Expr
collapsNegate (OperateUnary Negate (OperateUnary Negate e)) = return e
collapsNegate e                                             = fail (show e)

swapConstGet :: Expr -> Rule Expr
swapConstGet (OperateBinary Add a@(Value (Const _)) b@(Value (Get _))) = return $ OperateBinary Add b a
swapConstGet (OperateBinary Mul a@(Value (Const _)) b@(Value (Get _))) = return $ OperateBinary Mul b a
swapConstGet e                                                         = fail (show e)

swapConstDown :: Expr -> Rule Expr
swapConstDown
  (OperateBinary Add
    a@(Value (Const _))
      (OperateBinary Add
        b@(Value (Get _))
        c)) = return $ OperateBinary Add b (OperateBinary Add a c)
swapConstDown
  (OperateBinary Mul
    a@(Value (Const _))
      (OperateBinary Mul
        b@(Value (Get _))
        c)) = return $ OperateBinary Mul b (OperateBinary Mul a c)
swapConstDown e = fail (show e)

rotateBinary :: Expr -> Rule Expr
rotateBinary (OperateBinary Add (OperateBinary Add a b) c) = return $ OperateBinary Add a (OperateBinary Add b c)
rotateBinary (OperateBinary Mul (OperateBinary Mul a b) c) = return $ OperateBinary Mul a (OperateBinary Mul b c)
rotateBinary e                                             = fail (show e)
