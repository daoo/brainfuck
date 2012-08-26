module Brainfuck.Interpreter where

import Brainfuck.Data.Expr
import Brainfuck.Data.IL
import Brainfuck.Data.State
import Data.ListZipper
import Data.Sequence

run :: (Integral a) => State a -> [IL] -> State a
run = foldl evalOp

evalOp :: (Integral a) => State a -> IL -> State a
evalOp state@(State inp out mem) x = case x of
  While e ys -> until (isZero e) (`run` ys) state
  If e ys    -> if isZero e state then run state ys else state

  PutChar e -> State inp        (out |> evalExpr' e) mem
  GetChar d -> State (tail inp) out                  (applyAt' (head inp) d mem)
  Set d e   -> State inp        out                  (applyAt' (evalExpr' e) d mem)
  Shift s   -> State inp        out                  (move s mem)

  where
    isZero e = (== 0) . (`evalExpr` e) . flip peek . getMemory

    evalExpr' = evalExpr (`peek` mem)
    applyAt'  = applyAt . const

evalExpr :: (Integral a) => (Int -> a) -> Expr -> a
evalExpr f e = case e of
  Const v   -> fromIntegral v
  Get o     -> f o
  Add e1 e2 -> evalExpr f e1 + evalExpr f e2
  Mul e1 e2 -> evalExpr f e1 * evalExpr f e2
