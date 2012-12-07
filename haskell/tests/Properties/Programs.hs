module Properties.Programs where

import Code
import Data.Char
import Test.QuickCheck hiding (output)
import Tests.Programs

propReverse :: Property
propReverse = forAll printableString $ \str -> out str == reverse str
  where
    out str = run2 (str ++ "\NUL") bfReverse

propASCIIValues :: Property
propASCIIValues = forAll printableString $ \str -> values str == map ord str
  where
    out str = run2 (str ++ "\NUL") bfASCIIValues
    values  = map length . words . out
