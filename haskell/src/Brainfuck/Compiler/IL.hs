module Brainfuck.Compiler.IL where

import Test.QuickCheck

import Brainfuck.Compiler.Expr

data IL = Loop Int [IL]
        | Add Int Expr
        | Set Int Expr
        | Shift Int
        | PutChar Int
        | GetChar Int
  deriving (Eq)

instance Show IL where
  show loop@(Loop _ _) = showList [loop] ""
  show (Add d e)       = "Add " ++ show d ++ " " ++ show e
  show (Set d e)       = "Set " ++ show d ++ " " ++ show e
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
    oneof $ map return [Add i1 $ Const i2, Shift i1]

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                                    = []
filterIL f (Loop i loop : ils) | f (Loop i loop) = Loop i (filterIL f loop) : filterIL f ils
filterIL f (il : ils)          | f il            = il : filterIL f ils
filterIL f (_ : ils)                             = filterIL f ils

mapIL :: (IL -> IL) -> [IL] -> [IL]
mapIL _ []                 = []
mapIL f (Loop i loop : as) = f (Loop i (mapIL f loop)) : mapIL f as
mapIL f (a : as)           = f a : mapIL f as

modifyOffset :: (Int -> Int) -> IL -> IL
modifyOffset f il = case il of
  Loop d ils -> Loop (f d) ils
  Add d e    -> Add (f d) $ modifyPtr f e
  Set d e    -> Set (f d) $ modifyPtr f e
  Shift s    -> Shift s
  GetChar d  -> GetChar $ f d
  PutChar d  -> PutChar $ f d
