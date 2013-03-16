{-# LANGUAGE LambdaCase #-}
module Brainfuck.Interpret where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Data.Char
import Data.ListZipper
import Data.Sequence
import Data.Word

data State = State
  { input :: [Word8]
  , output :: Seq Word8
  , memory :: ListZipper Word8
  } deriving (Show, Eq)

newMemory :: ListZipper Word8
newMemory = ListZipper zeros 0 zeros
  where zeros = repeat 0

newState :: String -> State
newState inp = State (map (fromIntegral . ord) inp) empty newMemory

run :: State -> AST -> State
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
      If e    -> if isZero e state' then state' else run state' inner

    evalFunction state' = \case
      PutChar e  -> out (evalExpr' (memory state') e) state'
      GetChar d  -> finput tail $ fmem (set (head (input state')) d) state'
      Assign d e -> fmem (set (evalExpr' (memory state') e) d) state'
      Shift s    -> fmem (move s) state'

    isZero e = (== 0) . (`evalExpr` e) . flip peek . memory

    evalExpr' mem = evalExpr (`peek` mem)
    set           = applyAt . const

    out x s    = s { output = output s |> x }
    finput f s = s { input  = f (input s) }
    fmem f s   = s { memory = f (memory s) }

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
