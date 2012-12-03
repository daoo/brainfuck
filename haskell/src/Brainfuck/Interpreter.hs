{-# LANGUAGE LambdaCase #-}
module Brainfuck.Interpreter where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Data.Char
import Data.ListZipper
import Data.Sequence

data State a = State
  { input :: [a]
  , output :: Seq a
  , memory :: ListZipper a
  } deriving (Show, Eq)

chrIntegral :: (Integral a) => a -> Char
chrIntegral = chr . fromIntegral

ordIntegral :: (Integral a) => Char -> a
ordIntegral = fromIntegral . ord

newMemory :: (Integral a) => ListZipper a
newMemory = ListZipper zeros 0 zeros
  where zeros = repeat 0

newState :: (Integral a) => String -> State a
newState inp = State (map ordIntegral inp) empty newMemory

run :: (Integral a) => State a -> AST -> State a
run state = \case
  Nop                  -> state
  Instruction fun next -> run (evalFunction state fun) next
  Flow ctrl inner next -> run (evalFlow state inner ctrl) next
  where
    evalFlow state' inner = \case
      Forever -> error "infinite loop"
      Never   -> state'
      Once    -> run state' inner
      While e -> until (isZero e) (`run` inner) state'
      If e    -> if isZero e state' then run state' inner else state'

    evalFunction state'@(State inp out mem) = \case
      PutChar e -> state' { output = out |> evalExpr' mem e }
      GetChar d -> state' { input  = tail inp, memory = applyAt' (head inp) d mem }
      Set d e   -> state' { memory = applyAt' (evalExpr' mem e) d mem }
      Shift s   -> state' { memory = move s mem }

    isZero e = (== 0) . (`evalExpr` e) . flip peek . memory

    evalExpr' mem = evalExpr (`peek` mem)
    applyAt'      = applyAt . const

evalExpr :: (Integral a) => (Int -> a) -> Expr -> a
evalExpr f = unfold unary binary value
  where
    unary op a = case op of
      Id     -> a
      Negate -> -a

    binary op a b = case op of
      Add -> a + b
      Mul -> a * b

    value = \case
      Const v -> fromIntegral v
      Get o   -> f o
