module Brainfuck.Compiler.IL where

import Test.QuickCheck

data IL = Loop Int [IL]
        | Poke Int Int
        | Shift Int
        | PutChar Int
        | GetChar Int
  deriving (Eq)

instance Show IL where
  show loop@(Loop _ _) = showList [loop] ""
  show (Poke d i)      = "Poke " ++ show d ++ " " ++ show i
  show (Shift i)       = "Shift " ++ show i
  show (PutChar d)     = "PutChar " ++ show d
  show (GetChar d)     = "GetChar " ++ show d

  showList = helper ""
    where
      helper _ []                  = showString ""
      helper s (Loop i loop : ils) = showString s
                                   . showString "Loop "
                                   . showString (show i)
                                   . showString "\n"
                                   . helper (indent s) loop
                                   . helper s ils
      helper s (il : ils) = showString s
                          . shows il
                          . showString "\n"
                          . helper s ils

      indent s = ' ' : ' ' : s

instance Arbitrary IL where
  -- TODO: Random loops
  arbitrary = do
    i1 <- choose (-100, 100)
    i2 <- choose (-100, 100)
    oneof $ map return [Poke i1 i2, Shift i1]

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                     = []
filterIL f (Loop i loop : ils)    = Loop i (filterIL f loop) : filterIL f ils
filterIL f (il : ils) | f il      = il : filterIL f ils
                      | otherwise = filterIL f ils

mapIL :: (IL -> IL) -> [IL] -> [IL]
mapIL _ []                 = []
mapIL f (Loop i loop : as) = f (Loop i (mapIL f loop)) : mapIL f as
mapIL f (a : as)           = f a : mapIL f as

modifyRelative :: (Int -> Int) -> IL -> IL
modifyRelative f il = case il of
  PutChar d  -> PutChar $ f d
  GetChar d  -> GetChar $ f d
  Poke d i   -> Poke (f d) i
  Loop d ils -> Loop (f d) ils
  _          -> il

loopDepth :: IL -> Int
loopDepth (Loop _ ils) = (+1) $ maximum $ map loopDepth ils
loopDepth _            = 0
