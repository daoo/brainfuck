module Properties.Compiler where

import Brainfuck.Compile
import Brainfuck.Data.Brainfuck
import Brainfuck.Parse
import Control.Applicative
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BS

instance Arbitrary Token where
  arbitrary = oneof $ map return [Plus, Minus, ShiftRight, ShiftLeft, Input, Output]

instance Arbitrary Brainfuck where
  arbitrary = frequency
    [ (5, return Nop)
    , (20, Token <$> arbitrary <*> arbitrary)
    , (1, Repeat <$> arbitrary <*> arbitrary)
    ]

propCompileDecompile :: Brainfuck -> Bool
propCompileDecompile bf = bf == decompile (compile bf)

propParser :: Brainfuck -> Bool
propParser bf = bf == parseBrainfuck (BS.pack $ show bf)
