module Brainfuck.Compiler.IL where

import qualified Brainfuck.Parser.Brainfuck as B

import Brainfuck.Ext

data MemShift = ShiftLeft Int | ShiftRight Int
  deriving Show

data IL = Loop [IL]
        | Poke Int Int
        | Shift MemShift
        | PutChar Int
        | GetChar Int
  deriving Show

compile :: [B.Brainfuck] -> [IL]
compile []                  = []
compile (B.Loop []:bs)      = compile bs
compile (B.Loop l:bs)       = Loop (compile l) : compile bs
compile bf@(B.Token tok:bs) = case tok of
  B.Plus       -> Poke 0 p : compile bsp
  B.Minus      -> Poke 0 p : compile bsp
  B.ShiftRight -> shift : compile bss
  B.ShiftLeft  -> shift : compile bss
  B.Output     -> PutChar 0 : compile bs
  B.Input      -> GetChar 0 : compile bs
  where
    shift = Shift $ if s < 0
      then ShiftLeft (abs s)
      else ShiftRight s

    (p, bsp) = pokes bf
    (s, bss) = shifts bf

shifts :: [B.Brainfuck] -> (Int, [B.Brainfuck])
shifts (B.Token B.ShiftRight:bs) = mapFst (+1) $ shifts bs
shifts (B.Token B.ShiftLeft:bs)  = mapFst (subtract 1) $ shifts bs
shifts bs                        = (0, bs)

pokes :: [B.Brainfuck] -> (Int, [B.Brainfuck])
pokes (B.Token B.Plus:bs)  = mapFst (+1) $ pokes bs
pokes (B.Token B.Minus:bs) = mapFst (subtract 1) $ pokes bs
pokes bs                   = (0, bs)

