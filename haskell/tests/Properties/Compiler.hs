module Properties.Compiler where

import Brainfuck.Compile
import Brainfuck.Data.Brainfuck
import Brainfuck.Parse
import qualified Data.ByteString.Char8 as BS

propCompileDecompile :: Brainfuck -> Bool
propCompileDecompile bf = bf == decompile (compile bf)

propParser :: Brainfuck -> Bool
propParser bf = bf == parseBrainfuck (BS.pack $ show bf)
