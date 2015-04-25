module Properties.Compiler where

import Brainfuck.Compile
import Brainfuck.Data.Brainfuck
import Brainfuck.Parse
import Data.ByteString (pack)

propCompileDecompile :: Brainfuck -> Bool
propCompileDecompile bf = bf == decompile (compile bf)

propParser :: Brainfuck -> Bool
propParser bf = bf == parseBrainfuck (pack $ show bf)
