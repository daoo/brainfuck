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
evalOp state@(State inp out mem) x = case x of
  While e ys -> until ((== 0) . evalExpr e . flip peek . getMemory) (`run` ys) state
  If e ys    -> if 0 == evalExpr e (`peek` mem) then run state ys else state

  PutChar e -> State inp        (out |> evalExpr' e) mem
  GetChar d -> State (tail inp) out                  (applyIndex' (head inp) d mem)
  Set d e   -> State inp        out                  (applyIndex' (evalExpr' e) d mem)
  Shift s   -> State inp        out                  (move s mem)

  where
    evalExpr' e = evalExpr e (`peek` mem)
    applyIndex' = applyIndex . const

evalExpr :: (Integral a) => Expr -> (Int -> a) -> a
evalExpr (Const v) _   = fromIntegral v
evalExpr (Get o) f     = f o
evalExpr (Add e1 e2) f = evalExpr e1 f + evalExpr e2 f
evalExpr (Mul e1 e2) f = evalExpr e1 f * evalExpr e2 f

applyIndex :: (a -> a) -> Int -> ListZipper a -> ListZipper a
applyIndex f 0 (ListZipper xs y zz)             = ListZipper xs (f y) zz
applyIndex f n (ListZipper xs y zz) | n < 0     = ListZipper (mapIndex f (abs n - 1) xs) y zz
                                    | otherwise = ListZipper xs y (mapIndex f (n - 1) zz)
