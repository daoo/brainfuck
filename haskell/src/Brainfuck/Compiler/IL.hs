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
compile []                  = []
compile ((BFLoop l):bs)     = Loop (compile l) : compile bs
compile ((BFToken Plus):bs) = Poke (1 + i) : compile bs'
  where (i, bs') = pokes bs
compile ((BFToken Minus):bs) = Poke (i - 1) : compile bs'
  where (i, bs') = pokes bs
compile ((BFToken ShiftRight):bs) = Shift (1 + i) : compile bs'
  where (i, bs') = shifts bs
compile ((BFToken ShiftLeft):bs) = Shift (i - 1) : compile bs'
  where (i, bs') = shifts bs
compile ((BFToken Output):bs) = PutChar : compile bs
compile ((BFToken Input):bs) = GetChar : compile bs

shifts :: [Brainfuck] -> (Int, [Brainfuck])
shifts (BFToken ShiftRight:bs) = mapFst inc $ shifts bs
shifts (BFToken ShiftLeft:bs)  = mapFst dec $ shifts bs
shifts bs                      = (0, bs)

pokes :: [Brainfuck] -> (Int, [Brainfuck])
pokes (BFToken Plus:bs)  = mapFst inc $ pokes bs
pokes (BFToken Minus:bs) = mapFst dec $ pokes bs
pokes bs                 = (0, bs)

