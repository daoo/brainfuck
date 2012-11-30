module Compiler where

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.Parser
import Brainfuck.Data.Brainfuck

propCompileDecompile :: [Brainfuck] -> Bool
propCompileDecompile bf = bf == decompile (compile bf)

propParser :: [Brainfuck] -> Bool
propParser bf = case parseBrainfuck (show bf) of
  Left _    -> False
  Right bf' -> bf == bf'
