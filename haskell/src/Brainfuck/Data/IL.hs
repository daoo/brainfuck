module Brainfuck.Data.IL where

import Brainfuck.Data.Expr
import Data.List
import Test.QuickCheck

data IL = While Expr [IL]
        | If Expr [IL]
        | Set Int Expr
        | Shift Int
        | PutChar Expr
        | GetChar Int
  deriving (Eq, Show)

instance Arbitrary IL where
  arbitrary = do
    i <- choose (-4, 10)
    e <- arbitrary
    frequency [ (4, return $ Set i e)
              , (2, return $ Shift i)
              , (1, return $ PutChar e)
              ]

  shrink (While e ys) = map (While e) $ tail $ tails ys
  shrink (If e ys)    = map (If e) $ tail $ tails ys
  shrink (Set d e)    = map (Set d) $ shrink e
  shrink (Shift i)    = map Shift $ shrink i
  shrink (PutChar e)  = map PutChar $ shrink e
  shrink (GetChar d)  = map GetChar $ shrink d

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                          = []
filterIL f (x@(While e ys) : xs) | f x = While e (filterIL f ys) : filterIL f xs
filterIL f (x@(If e ys) : xs)    | f x = If e (filterIL f ys) : filterIL f xs
filterIL f (x : xs)              | f x = x : filterIL f xs
filterIL f (_ : xs)                    = filterIL f xs

mapIL :: (IL -> IL) -> [IL] -> [IL]
mapIL = map . g
  where
    g f (While e ys) = f $ While e (map (g f) ys)
    g f (If e ys)    = f $ If e (map (g f) ys)
    g f il           = f il

modifyPtr :: (Int -> Int) -> IL -> IL
modifyPtr f x = case x of
  While e ys -> While (h e) (map (modifyPtr f) ys)
  If e ys    -> If (h e) (map (modifyPtr f) ys)
  Set d e    -> Set (f d) (h e)
  Shift s    -> Shift s
  PutChar e  -> PutChar (h e)
  GetChar d  -> GetChar (f d)

  where
    g (Get d) = Get $ f d
    g e       = e

    h = modifyLeafs g
