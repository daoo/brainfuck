module Utility
  ( wnull
  , wspace
  , printWord8
  , printInput
  , PrettyTarpit (..)
  , testCode
  , checkTransform
  , expectedOutput
  ) where

import Brainfuck.CodeGen.Indented
import Brainfuck.Data.Tarpit
import Brainfuck.Interpret
import Control.Monad
import Data.Char
import Data.Foldable
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

newtype PrettyTarpit = PrettyTarpit { getAst :: Tarpit }

instance Show PrettyTarpit where
  show (PrettyTarpit ast) = writeCode $ writeIndented ast

testCode :: Tarpit -> Tarpit -> Bool
testCode ast ast' = run [] ast == run [] ast'

checkTransform :: (Tarpit -> Tarpit) -> Tarpit -> Bool
checkTransform f ast = testCode ast (f ast)

expectedOutput :: Tarpit -> Input -> Input -> Bool
expectedOutput code inp out = toList (run inp code) == out
