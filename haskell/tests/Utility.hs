module Utility
  ( wnull
  , wspace
  , printNum
  , printInput
  , tests
  , testOutput
  , testMemory
  , expectedOutput
  ) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Interpret
import Control.Monad
import Data.Char
import Data.Foldable hiding (all)
import Data.ListZipper
import Test.QuickCheck

instance Num Expr where
  fromInteger = Const . fromInteger

  (+) = add

  (*)    = undefined
  abs    = undefined
  signum = undefined

wnull, wspace :: Num n => n
wnull  = 0
wspace = 32

printNum :: Num n => Gen n
printNum = frequency
  [ (5, f ['a'..'z'])
  , (4, f ['A'..'Z'])
  , (4, f ['0'..'9'])
  , (1, return wspace)
  ]
  where
    f = oneof . map (return . fromIntegral . ord)

printInput :: Gen Input
printInput = sized $ \n -> choose (0, n) >>= (`replicateM` printNum)

tests :: [MachineState -> MachineState -> Bool] -> Tarpit -> Tarpit -> Bool
tests fs a b = let a' = run [] a
                   b' = run [] b
                in all (\f -> f a' b') fs

testOutput :: MachineState -> MachineState -> Bool
testOutput a b = moutput a == moutput b

testMemory :: Int -> MachineState -> MachineState -> Bool
testMemory i a b = cut i (mmemory a) == cut i (mmemory b)

expectedOutput :: Tarpit -> Input -> Input -> Bool
expectedOutput code inp out = toList (exec inp code) == out
