{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Expr where

import Brainfuck.Data.Expr
import Brainfuck.Optimization.Rewriting

exprRules :: [Expr -> Rule Expr]
exprRules =
  [ addConsts
  , mulConsts
  , idAny
  , negCollaps
  , negConstant
  , addZeroL
  , addZeroR
  , mulOneL
  , mulOneR
  , swapConstGet
  , swapConstDown
  , rotateBinary
  ]

addConsts :: Expr -> Rule Expr
addConsts (OperateBinary Add (Value (Const a)) (Value (Const b))) = return $ mkInt (a + b)
addConsts e                                                       = fail (show e)

addZeroL :: Expr -> Rule Expr
addZeroL (OperateBinary Add (Value (Const 0)) b) = return b
addZeroL e                                       = fail (show e)

addZeroR :: Expr -> Rule Expr
addZeroR (OperateBinary Add a (Value (Const 0))) = return a
addZeroR e                                       = fail (show e)

mulConsts :: Expr -> Rule Expr
mulConsts (OperateBinary Mul (Value (Const a)) (Value (Const b))) = return $ mkInt (a * b)
mulConsts e                                                       = fail (show e)

mulOneL :: Expr -> Rule Expr
mulOneL (OperateBinary Mul (Value (Const 1)) b) = return b
mulOneL e                                       = fail (show e)

mulOneR :: Expr -> Rule Expr
mulOneR (OperateBinary Mul a (Value (Const 1))) = return a
mulOneR e                                       = fail (show e)

negConstant :: Expr -> Rule Expr
negConstant (OperateUnary Negate (Value (Const a))) = return $ mkInt (-a)
negConstant e                                       = fail (show e)

negCollaps :: Expr -> Rule Expr
negCollaps (OperateUnary Negate (OperateUnary Negate e)) = return e
negCollaps e                                             = fail (show e)

idAny :: Expr -> Rule Expr
idAny (OperateUnary Id e) = return e
idAny e                   = fail (show e)

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
