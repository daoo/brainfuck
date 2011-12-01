module Brainfuck.Compiler.IL where

import Brainfuck.Parser.Brainfuck

import Brainfuck.Ext

data IL = Loop [IL]
        | Poke Int
        | Shift Int
        | PutChar
        | GetChar
  deriving Show

compile :: [Brainfuck] -> [IL]
compile []                 = []
compile ((BFLoop []):bs)   = compile bs
compile ((BFLoop l):bs)    = Loop (compile l) : compile bs
compile ((BFToken tok):bs) = case tok of
  Plus       -> Poke (1 + p) : compile bsp
  Minus      -> Poke (p - 1) : compile bsp
  ShiftRight -> Shift (1 + s) : compile bss
  ShiftLeft  -> Shift (s - 1) : compile bss
  Output     -> PutChar : compile bs
  Input      -> GetChar : compile bs
  where
    (p, bsp) = pokes bs
    (s, bss) = shifts bs

shifts :: [Brainfuck] -> (Int, [Brainfuck])
shifts (BFToken ShiftRight:bs) = mapFst (+1) $ shifts bs
shifts (BFToken ShiftLeft:bs)  = mapFst (subtract 1) $ shifts bs
shifts bs                      = (0, bs)

pokes :: [Brainfuck] -> (Int, [Brainfuck])
pokes (BFToken Plus:bs)  = mapFst (+1) $ pokes bs
pokes (BFToken Minus:bs) = mapFst (subtract 1) $ pokes bs
pokes bs                 = (0, bs)

