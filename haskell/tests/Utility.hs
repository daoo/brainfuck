module Utility
  ( wnull
  , wspace
  , printWord8
  , printInput
  , assignments
  , PrettyTarpit (..)
  , tests
  , testOutput
  , testMemory
  , expectedOutput
  ) where

import Brainfuck.CodeGen.Indented
import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Interpret
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable hiding (all)
import Data.ListZipper
import Data.Word
import Test.QuickCheck
import Text.CodeWriter

wnull, wspace :: Word8
wnull  = 0
wspace = 32

printWord8 :: Gen Word8
printWord8 = frequency
  [ (5, f ['a'..'z'])
  , (4, f ['A'..'Z'])
  , (4, f ['0'..'9'])
  , (1, return wspace)
  ]
  where
    f = oneof . map (return . fromIntegral . ord)

printInput :: Gen Input
printInput = sized $ \n -> choose (0, n) >>= (`replicateM` printWord8)

assignments :: Gen Tarpit
assignments = sized go
  where
    go 0 = return Nop
    go n = Instruction <$> assignment <*> go (n `div` 2)

    assignment = Assign <$> choose (-10, 10) <*> arbitrary

instance Arbitrary Function where
  arbitrary = oneof
    [ Assign <$> choose (-10, 10) <*> arbitrary
    , Shift <$> choose (-10, 10)
    , PutChar <$> arbitrary
    , GetChar <$> choose (-10, 10)
    ]

  shrink (Assign d e) = Assign 0 (constant 0) : map (Assign d) (shrink e)
  shrink (Shift s)    = Shift 0 : map Shift (shrink s)
  shrink (PutChar e)  = PutChar (constant 0) : map PutChar (shrink e)
  shrink (GetChar d)  = map GetChar $ shrink d

instance Arbitrary Tarpit where
  arbitrary = return Nop

  shrink Nop                    = []
  shrink (Instruction fun next) = zipWith Instruction (shrink fun) (shrink next)
  shrink (Flow ctrl inner next) = Flow ctrl inner Nop : shrink next

newtype PrettyTarpit = PrettyTarpit { getTarpit :: Tarpit }

instance Show PrettyTarpit where
  show (PrettyTarpit code) = writeCode $ writeIndented code

tests :: [Machine -> Machine -> Bool] -> Tarpit -> Tarpit -> Bool
tests fs a b = let a' = run [] a
                   b' = run [] b
                in all (\f -> f a' b') fs

testOutput :: Machine -> Machine -> Bool
testOutput a b = moutput a == moutput b

testMemory :: Int -> Machine -> Machine -> Bool
testMemory i a b = takeBoth i (mmemory a) == takeBoth i (mmemory b)

expectedOutput :: Tarpit -> Input -> Input -> Bool
expectedOutput code inp out = toList (exec inp code) == out
