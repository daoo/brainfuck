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
  , mulZeroL
  , mulZeroR
  , mulOneL
  , mulOneR
  , mulNegOneL
  , mulNegOneR
  , swapConstGet
  , swapConstDown
  , rotateBinary
  ]

addConsts :: Expr -> Rule Expr
addConsts (OperateBinary Add (Return (Const a)) (Return (Const b))) = return $ int (a + b)
addConsts e                                                         = fail (show e)

addZeroL :: Expr -> Rule Expr
addZeroL (OperateBinary Add (Return (Const 0)) b) = return b
addZeroL e                                        = fail (show e)

addZeroR :: Expr -> Rule Expr
addZeroR (OperateBinary Add a (Return (Const 0))) = return a
addZeroR e                                        = fail (show e)

mulConsts :: Expr -> Rule Expr
mulConsts (OperateBinary Mul (Return (Const a)) (Return (Const b))) = return $ int (a * b)
mulConsts e                                                         = fail (show e)

mulZeroL :: Expr -> Rule Expr
mulZeroL (OperateBinary Mul (Return (Const 0)) _) = return $ int 0
mulZeroL e                                        = fail (show e)

mulZeroR :: Expr -> Rule Expr
mulZeroR (OperateBinary Mul _ (Return (Const 0))) = return $ int 0
mulZeroR e                                        = fail (show e)

mulOneL :: Expr -> Rule Expr
mulOneL (OperateBinary Mul (Return (Const 1)) b) = return b
mulOneL e                                        = fail (show e)

mulOneR :: Expr -> Rule Expr
mulOneR (OperateBinary Mul a (Return (Const 1))) = return a
mulOneR e                                        = fail (show e)

mulNegOneL :: Expr -> Rule Expr
mulNegOneL (OperateBinary Mul (Return (Const (-1))) b) = return $ OperateUnary Negate b
mulNegOneL e                                           = fail (show e)

mulNegOneR :: Expr -> Rule Expr
mulNegOneR (OperateBinary Mul a (Return (Const (-1)))) = return $ OperateUnary Negate a
mulNegOneR e                                           = fail (show e)

negConstant :: Expr -> Rule Expr
negConstant (OperateUnary Negate (Return (Const a))) = return $ int (-a)
negConstant e                                        = fail (show e)

negCollaps :: Expr -> Rule Expr
negCollaps (OperateUnary Negate (OperateUnary Negate a)) = return a
negCollaps e                                             = fail (show e)

idAny :: Expr -> Rule Expr
idAny (OperateUnary Id a) = return a
idAny e                   = fail (show e)

swapConstGet :: Expr -> Rule Expr
swapConstGet (OperateBinary Add a@(Return (Const _)) b@(Return (Get _))) = return $ OperateBinary Add b a
swapConstGet (OperateBinary Mul a@(Return (Const _)) b@(Return (Get _))) = return $ OperateBinary Mul b a
swapConstGet e                                                           = fail (show e)

swapConstDown :: Expr -> Rule Expr
swapConstDown
  (OperateBinary Add
    a@(Return (Const _))
      (OperateBinary Add
        b@(Return (Get _))
        c)) = return $ OperateBinary Add b (OperateBinary Add a c)
swapConstDown
  (OperateBinary Mul
    a@(Return (Const _))
      (OperateBinary Mul
        b@(Return (Get _))
        c)) = return $ OperateBinary Mul b (OperateBinary Mul a c)
swapConstDown e = fail (show e)

rotateBinary :: Expr -> Rule Expr
rotateBinary (OperateBinary Add (OperateBinary Add a b) c) = return $ OperateBinary Add a (OperateBinary Add b c)
rotateBinary (OperateBinary Mul (OperateBinary Mul a b) c) = return $ OperateBinary Mul a (OperateBinary Mul b c)
rotateBinary e                                             = fail (show e)
