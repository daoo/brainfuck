module Brainfuck.Compiler.Brainfuck (compile, decompile) where

import Brainfuck.Compiler.IL
import Brainfuck.Parser.Brainfuck

compile :: [Brainfuck] -> [IL]
compile []               = []
compile (Repeat l : bs)  = Loop 0 (compile l) : compile bs
compile (Token tok : bs) = tok' : compile bs
  where
    tok' = case tok of
      Plus       -> Poke 0 1
      Minus      -> Poke 0 (-1)
      ShiftRight -> Shift 1
      ShiftLeft  -> Shift (-1)
      Output     -> PutChar 0
      Input      -> GetChar 0

decompile :: [IL] -> [Brainfuck]
decompile []              = []
decompile (Loop i l : il) = surround i $ Repeat (decompile l) : decompile il
decompile (tok : il)      = token ++ decompile il
  where
    token = case tok of
      Poke d i  -> surround d (makePokes i)
      Shift s   -> makeShifts s
      PutChar d -> surround d [Token Output]
      GetChar d -> surround d [Token Input]
      _         -> error "Should not happen"

surround :: Int -> [Brainfuck] -> [Brainfuck]
surround 0 ts = ts
surround d ts = concat [makeShifts d, ts, makeShifts (negate d)]

makeShifts :: Int -> [Brainfuck]
makeShifts d | d == 0    = []
             | d < 0     = replicate (abs d) (Token ShiftLeft)
             | otherwise = replicate d (Token ShiftRight)

makePokes :: Int -> [Brainfuck]
makePokes i | i == 0    = []
            | i < 0     = replicate (abs i) (Token Minus)
            | otherwise = replicate i (Token Plus)
