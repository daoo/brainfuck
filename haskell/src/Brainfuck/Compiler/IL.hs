module Brainfuck.Compiler.IL where

import Test.QuickCheck

import qualified Brainfuck.Parser.Brainfuck as B

data IL = Loop [IL]
        | Poke Int Int
        | Shift Int
        | PutChar Int
        | GetChar Int
  deriving (Eq)

instance Show IL where
  show loop@(Loop _) = showList [loop] ""
  show (Poke d i)    = "Poke " ++ show d ++ " " ++ show i
  show (Shift i)     = "Shift " ++ show i
  show (PutChar d)   = "PutChar " ++ show d
  show (GetChar d)   = "GetChar " ++ show d

  showList = helper ""
    where
      helper _ []                = showString ""
      helper s (Loop loop : ils) = showString s . showString "Loop\n" . helper (indent s) loop . helper s ils
      helper s (il : ils)        = showString s . shows il . showString "\n" . helper s ils 

      indent s = ' ' : ' ' : s

instance Arbitrary IL where
  -- TODO: Random loops
  arbitrary = do
    i1 <- choose (-100, 100)
    i2 <- choose (-100, 100)
    oneof $ map return [Poke i1 i2, Shift i1]

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                     = []
filterIL f (Loop loop : ils)      = Loop (filterIL f loop) : filterIL f ils
filterIL f (il : ils) | f il      = il : filterIL f ils
                      | otherwise = filterIL f ils

mapIL :: (IL -> IL) -> [IL] -> [IL]
mapIL _ []               = []
mapIL f (Loop loop : as) = Loop (mapIL f loop) : mapIL f as
mapIL f (a : as)         = f a : mapIL f as

compile :: [B.Brainfuck] -> [IL]
compile []                 = []
compile (B.Loop l : bs)    = Loop (compile l) : compile bs
compile (B.Token tok : bs) = tok' : compile bs
  where
    tok' = case tok of
      B.Plus       -> Poke 0 1
      B.Minus      -> Poke 0 (-1)
      B.ShiftRight -> Shift 1
      B.ShiftLeft  -> Shift (-1)
      B.Output     -> PutChar 0
      B.Input      -> GetChar 0

decompile :: [IL] -> [B.Brainfuck]
decompile []            = []
decompile (Loop l : il) = B.Loop (decompile l) : decompile il
decompile (tok : il)    = token ++ decompile il
  where
    token = case tok of
      Poke d i  -> ws d (mp i)
      Shift s   -> ms s
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

modifyRelative :: (Int -> Int) -> IL -> IL
modifyRelative f il = case il of
  PutChar d -> PutChar $ f d
  GetChar d -> GetChar $ f d
  Poke d i  -> Poke (f d) i
  --Shift i   -> Shift $ f i
  _         -> il

