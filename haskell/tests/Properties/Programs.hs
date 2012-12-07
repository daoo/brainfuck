module Properties.Programs where

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.Parser
import Brainfuck.Data.AST
import Brainfuck.Data.Brainfuck
import Brainfuck.Interpreter
import Brainfuck.Optimization.Pipeline
import Code
import Data.Char
import Data.Foldable (toList)
import Data.Word
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
