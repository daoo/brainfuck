module Brainfuck.Compiler.IL where

import Data.List
import Test.QuickCheck

import Brainfuck.Compiler.Expr

data IL = While Int [IL]
        | Set Int Expr
        | Shift Int
        | PutChar Expr
        | GetChar Int
  deriving (Eq, Show)

instance Arbitrary IL where
  arbitrary = do
    i <- choose (-4, 10)
    e <- arbitrary
    frequency [ (2, return $ Set i e)
              , (1, return $ Shift i) ]

  shrink (While i loop) = map (While i) $ tails loop
  shrink (Set d e)      = map (Set d) $ shrink e
  shrink (Shift i)      = map Shift [0 .. i - 1]
  shrink (PutChar e)    = map PutChar $ shrink e
  shrink (GetChar _)    = []

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                               = []
filterIL f (il@(While i loop) : ils) | f il = While i (filterIL f loop) : filterIL f ils
filterIL f (il : ils)                | f il = il : filterIL f ils
filterIL f (_ : ils)                        = filterIL f ils

mapIL :: (IL -> IL) -> [IL] -> [IL]
mapIL = map . g
  where
    g f (While i loop) = f $ While i (map (g f) loop)
    g f il             = f il

modifyOffset :: (Int -> Int) -> IL -> IL
modifyOffset f il = case il of
  While d ils -> While (f d) ils
  Set d e     -> Set (f d) $ modifyPtr f e
  Shift s     -> Shift s
  GetChar d   -> GetChar $ f d
  PutChar e   -> PutChar $ modifyPtr f e
