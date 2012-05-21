module Brainfuck.Interpreter.Interpreter where

import Data.Sequence
import Data.ListZipper

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL
import Brainfuck.Ext
import Brainfuck.Interpreter.State

run :: (Integral a) => State a -> [IL] -> State a
run = foldl evalOp

evalOp :: (Integral a) => State a -> IL -> State a
evalOp state@(State inp out mem) op = case op of
  While d ops -> until ((== 0) . peek d . getMemory) (`run` ops) state

  PutChar e -> State inp        (out |> evalExpr' e) mem
  GetChar d -> State (tail inp) out                  (applyIndex' (head inp) d mem)
  Set d e   -> State inp        out                  (applyIndex' (evalExpr' e) d mem)
  Shift s   -> State inp        out                  (move s mem)

  where
    evalExpr'   = evalExpr (`peek` mem)
    applyIndex' = applyIndex . const

evalExpr :: (Integral a) => (Int -> a) -> Expr -> a
evalExpr _ (Const v)   = fromIntegral v
evalExpr f (Get o)     = f o
evalExpr f (Add e1 e2) = evalExpr f e1 + evalExpr f e2
evalExpr f (Mul e1 e2) = evalExpr f e1 * evalExpr f e2

applyIndex :: (a -> a) -> Int -> ListZipper a -> ListZipper a
applyIndex f 0 (ListZipper xs y zz)             = ListZipper xs (f y) zz
applyIndex f n (ListZipper xs y zz) | n < 0     = ListZipper (mapIndex f (abs n) xs) y zz
                                    | otherwise = ListZipper xs y (mapIndex f n zz)
