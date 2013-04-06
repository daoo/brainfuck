module Utility
  ( wnull
  , wspace
  , printWord8
  , printInput
  , PrettyTarpit (..)
  , tests
  , testOutput
  , testMemory
  , expectedOutput
  ) where

import Brainfuck.CodeGen.Indented
import Brainfuck.Data.Tarpit
import Brainfuck.Interpret
import Control.Monad
import Data.Char
import Data.Foldable hiding (all)
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

newtype PrettyTarpit = PrettyTarpit { getTarpit :: Tarpit }

instance Show PrettyTarpit where
  show (PrettyTarpit code) = writeCode $ writeIndented code

tests :: [Machine -> Machine -> Bool] -> Tarpit -> Tarpit -> Bool
tests fs a b = let a' = run [] a
                   b' = run [] b
                in all (\f -> f a' b') fs

testOutput :: Machine -> Machine -> Bool
testOutput a b = moutput a == moutput b

testMemory :: Machine -> Machine -> Bool
testMemory a b = mmemory a == mmemory b

expectedOutput :: Tarpit -> Input -> Input -> Bool
expectedOutput code inp out = toList (exec inp code) == out
