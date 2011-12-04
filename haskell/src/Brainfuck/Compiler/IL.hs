module Brainfuck.Compiler.IL where

import Brainfuck.Parser.Brainfuck

import Brainfuck.Ext

data IL = Loop [IL]
        | Poke Int Int
        | RightShifts Int
        | LeftShifts Int
        | PutChar Int
        | GetChar Int
  deriving Show

compile :: [Brainfuck] -> [IL]
compile []                    = []
compile ((BFLoop []):bs)      = compile bs
compile ((BFLoop l):bs)       = Loop (compile l) : compile bs
compile bf@((BFToken tok):bs) = case tok of
  Plus       -> Poke 0 p : compile bsp
  Minus      -> Poke 0 p : compile bsp
  ShiftRight -> shift : compile bss
  ShiftLeft  -> shift : compile bss
  Output     -> PutChar 0 : compile bs
  Input      -> GetChar 0 : compile bs
  where
    shift = if s < 0
              then LeftShifts (abs s)
              else RightShifts s

    (p, bsp) = pokes bf
    (s, bss) = shifts bf

shifts :: [Brainfuck] -> (Int, [Brainfuck])
shifts (BFToken ShiftRight:bs) = mapFst (+1) $ shifts bs
shifts (BFToken ShiftLeft:bs)  = mapFst (subtract 1) $ shifts bs
shifts bs                      = (0, bs)

pokes :: [Brainfuck] -> (Int, [Brainfuck])
pokes (BFToken Plus:bs)  = mapFst (+1) $ pokes bs
pokes (BFToken Minus:bs) = mapFst (subtract 1) $ pokes bs
pokes bs                 = (0, bs)

