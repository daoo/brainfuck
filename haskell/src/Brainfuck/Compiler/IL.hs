module Brainfuck.Compiler.IL where

import Test.QuickCheck

import qualified Brainfuck.Parser.Brainfuck as B

data MemShift = ShiftLeft Int | ShiftRight Int
  deriving (Show, Eq)

fromInt :: Int -> MemShift
fromInt i = if i < 0
  then ShiftLeft (abs i)
  else ShiftRight i

shiftCount :: MemShift -> Int
shiftCount (ShiftLeft i)  = negate i
shiftCount (ShiftRight i) = i

data IL = Loop [IL]
        | Poke Int Int
        | Shift MemShift
        | PutChar Int
        | GetChar Int
  deriving (Show, Eq)

instance Arbitrary IL where
  arbitrary = do
    i1 <- choose (-100, 100)
    i2 <- choose (-100, 100)
    oneof $ map return [Poke i1 i2, Shift (fromInt i1)]

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                     = []
filterIL f (Loop loop : ils)      = Loop (filterIL f loop) : filterIL f ils
filterIL f (il : ils) | f il      = il : filterIL f ils
                      | otherwise = filterIL f ils

compile :: [B.Brainfuck] -> [IL]
compile []                 = []
compile (B.Loop l : bs)    = Loop (compile l) : compile bs
compile (B.Token tok : bs) = tok' : compile bs
  where
    tok' = case tok of
      B.Plus       -> Poke 0 1
      B.Minus      -> Poke 0 (-1)
      B.ShiftRight -> Shift (ShiftRight 1)
      B.ShiftLeft  -> Shift (ShiftLeft 1)
      B.Output     -> PutChar 0
      B.Input      -> GetChar 0

decompile :: [IL] -> [B.Brainfuck]
decompile []            = []
decompile (Loop l : il) = B.Loop (decompile l) : decompile il
decompile (tok : il)    = token ++ decompile il
  where
    token = case tok of
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

modifyRelative :: IL -> Int -> IL
modifyRelative (PutChar _) p = PutChar p
modifyRelative (GetChar _) p = GetChar p
modifyRelative (Poke _ i) p  = Poke p i
modifyRelative il _          = il
     
