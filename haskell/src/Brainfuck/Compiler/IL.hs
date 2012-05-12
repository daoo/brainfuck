module Brainfuck.Compiler.IL where

import Data.List
import Test.QuickCheck

import Brainfuck.Compiler.Expr

data IL = Loop Int [IL]
        | Set Int Expr
        | Shift Int
        | PutChar Expr
        | GetChar Int
  deriving (Eq)

instance Show IL where
  show loop@(Loop _ _) = showList [loop] ""
  show (Set d e)       = "Set " ++ show d ++ " " ++ show e
  show (Shift i)       = "Shift " ++ show i
  show (PutChar e)     = "PutChar " ++ show e
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
  arbitrary = do
    i <- choose (-4, 10)
    e <- arbitrary
    frequency [ (2, return $ Set i e)
              , (1, return $ Shift i) ]

  shrink (Loop i loop) = map (Loop i) $ tails loop
  shrink (Set d e)     = map (Set d) $ shrink e
  shrink (Shift i)     = map Shift [0 .. i - 1]
  shrink (PutChar e)   = map PutChar $ shrink e
  shrink (GetChar _)   = []

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                              = []
filterIL f (il@(Loop i loop) : ils) | f il = Loop i (filterIL f loop) : filterIL f ils
filterIL f (il : ils)               | f il = il : filterIL f ils
filterIL f (_ : ils)                       = filterIL f ils

mapIL :: (IL -> IL) -> [IL] -> [IL]
mapIL = map . g
  where
    g f (Loop i loop) = f $ Loop i (map (g f) loop)
    g f il            = f il

modifyOffset :: (Int -> Int) -> IL -> IL
modifyOffset f il = case il of
  Loop d ils -> Loop (f d) ils
  Set d e    -> Set (f d) $ modifyPtr f e
  Shift s    -> Shift s
  GetChar d  -> GetChar $ f d
  PutChar e  -> PutChar $ modifyPtr f e
