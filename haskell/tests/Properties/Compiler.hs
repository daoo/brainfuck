module Properties.Compiler where

import Brainfuck.Compile
import Brainfuck.Data.Brainfuck
import Brainfuck.Parse

propCompileDecompile :: [Brainfuck] -> Bool
propCompileDecompile bf = bf == decompile (compile bf)

propParser :: [Brainfuck] -> Bool
propParser bf = case parseBrainfuck (show bf) of
  Left _    -> False
  Right bf' -> bf == bf'
