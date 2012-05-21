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
evalOp state (While i ops)    = until ((== 0) . peek i . getMemory) (`run` ops) state
evalOp (State inp out mem) op = state'
  where
    state' = case op of
      PutChar e -> State inp (out |> (evalExpr (`peek` mem) e)) mem
      GetChar d -> State (tail inp) out $ applyIndex (const $ head inp) d mem
      Set d e   -> State inp out $ applyIndex (const $ evalExpr (`peek` mem) e) d mem
      Shift s   -> State inp out $ move s mem

      While _ _ -> error "Should not happen"

evalExpr :: (Integral a) => (Int -> a) -> Expr -> a
evalExpr _ (Const v)   = fromIntegral v
evalExpr f (Get o)     = f o
evalExpr f (Add e1 e2) = evalExpr f e1 + evalExpr f e2
evalExpr f (Mul e1 e2) = evalExpr f e1 * evalExpr f e2

applyIndex :: (a -> a) -> Int -> ListZipper a -> ListZipper a
applyIndex f 0 (ListZipper xs y zz)             = ListZipper xs (f y) zz
applyIndex f n (ListZipper xs y zz) | n < 0     = ListZipper (mapIndex f (abs n) xs) y zz
                                    | otherwise = ListZipper xs y (mapIndex f n zz)
