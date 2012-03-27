module Brainfuck.Compiler.IL where

import qualified Brainfuck.Parser.Brainfuck as B

import Brainfuck.Ext

data MemShift = ShiftLeft Int | ShiftRight Int
  deriving Show

fromInt :: Int -> MemShift
fromInt i = if i < 0
  then ShiftLeft (abs i)
  else ShiftRight i

shiftCount :: MemShift -> Int
shiftCount (ShiftLeft i)  = -i
shiftCount (ShiftRight i) = i

data IL = Loop [IL]
        | Poke Int Int
        | Shift MemShift
        | PutChar Int
        | GetChar Int
  deriving Show

compile :: [B.Brainfuck] -> [IL]
compile []                  = []
compile (B.Loop l : bs)     = Loop (compile l) : compile bs
compile bf@(B.Token tok:bs) = case tok of
  B.Plus       -> Poke 0 p : compile bsp
  B.Minus      -> Poke 0 p : compile bsp
  B.ShiftRight -> Shift (fromInt s) : compile bss
  B.ShiftLeft  -> Shift (fromInt s) : compile bss
  B.Output     -> PutChar 0 : compile bs
  B.Input      -> GetChar 0 : compile bs
  where
    (p, bsp) = pokes bf
    (s, bss) = shifts bf

decompile :: [IL] -> [B.Brainfuck]
decompile []            = []
decompile (Loop l : il) = B.Loop (decompile l) : decompile il
decompile (tok : il)    = token tok ++ decompile il
  where
    token tok = case tok of
      Poke d i  -> ws d (mp i)
      Shift s   -> ms (shiftCount s)
      PutChar d -> ws d [B.Token B.Output]
      GetChar d -> ws d [B.Token B.Input]
      _         -> error "Should not happen"

    ws 0 ts = ts
    ws d ts = concat [ms d, ts, ms (negate d)]

    ms d | d == 0    = []
         | d < 0     = replicate (abs d) (B.Token B.ShiftLeft)
         | otherwise = replicate d (B.Token B.ShiftRight)

    mp i | i == 0    = []
         | i < 0     = replicate (abs i) (B.Token B.Minus)
         | otherwise = replicate i (B.Token B.Plus)

shifts :: [B.Brainfuck] -> (Int, [B.Brainfuck])
shifts (B.Token B.ShiftRight:bs) = mapFst (+1) $ shifts bs
shifts (B.Token B.ShiftLeft:bs)  = mapFst (subtract 1) $ shifts bs
shifts bs                        = (0, bs)

pokes :: [B.Brainfuck] -> (Int, [B.Brainfuck])
pokes (B.Token B.Plus:bs)  = mapFst (+1) $ pokes bs
pokes (B.Token B.Minus:bs) = mapFst (subtract 1) $ pokes bs
pokes bs                   = (0, bs)

