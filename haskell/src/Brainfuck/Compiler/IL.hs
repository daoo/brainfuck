module Brainfuck.Compiler.IL where

import Data.List
import Test.QuickCheck

import Brainfuck.Compiler.Expr

data IL = While Int [IL]
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
    frequency [ (2, return $ Set i e)
              , (1, return $ Shift i) ]

  shrink (While d ys) = map (While d) $ tail $ tails ys
  shrink (If e ys)    = [If e' ys' | e' <- shrink e, ys' <- tail (tails ys)]
  shrink (Set d e)    = map (Set d) $ shrink e
  shrink (Shift i)    = map Shift [0 .. i - 1]
  shrink (PutChar e)  = map PutChar $ shrink e
  shrink (GetChar _)  = []

filterIL :: (IL -> Bool) -> [IL] -> [IL]
filterIL _ []                          = []
filterIL f (x@(While d ys) : xs) | f x = While d (filterIL f ys) : filterIL f xs
filterIL f (x@(If e ys) : xs)    | f x = If e (filterIL f ys) : filterIL f xs
filterIL f (x : xs)              | f x = x : filterIL f xs
filterIL f (_ : xs)                    = filterIL f xs

mapIL :: (IL -> IL) -> [IL] -> [IL]
mapIL = map . g
  where
    g f (While d ys) = f $ While d (map (g f) ys)
    g f (If e ys)    = f $ If e (map (g f) ys)
    g f il           = f il

modifyOffset :: (Int -> Int) -> IL -> IL
modifyOffset f x = case x of
  While d ys -> While (f d) ys
  If e ys    -> If (modifyPtr f e) ys
  Set d e    -> Set (f d) $ modifyPtr f e
  Shift s    -> Shift s
  GetChar d  -> GetChar $ f d
  PutChar e  -> PutChar $ modifyPtr f e
