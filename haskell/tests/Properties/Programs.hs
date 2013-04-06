module Properties.Programs
  ( propReverse
  , propASCIIValues
  ) where

import Data.List
import Programs
import Test.QuickCheck
import Utility

propReverse :: Property
propReverse = forAll printInput $ \inp ->
  expectedOutput (parseCompile bfReverse) (inp ++ [wnull]) (reverse inp)

propASCIIValues :: Property
propASCIIValues = forAll printInput $ \inp -> expectedOutput
  (parseCompile bfASCIIValues)
  (inp ++ [wnull])
  (spacify $ map bangs inp)
  where
    bangs = (`replicate` 33) . fromIntegral

    spacify [] = []
    spacify xs = intercalate [wspace] xs ++ [wspace]
