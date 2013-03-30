module Properties.Programs where

import Brainfuck.Interpret
import Code
import Data.Char
import Test.QuickCheck hiding (output)
import Tests.Programs

propReverse :: Property
propReverse = forAll printableString $ \str -> out str == reverse str
  where
    out str = run1 (str ++ "\NUL") $ parseCompile bfReverse

propASCIIValues :: Property
propASCIIValues = forAll printableString $ \str -> values str == map ord str
  where
    out str = run1 (str ++ "\NUL") $ parseCompile bfASCIIValues
    values  = map length . words . out
