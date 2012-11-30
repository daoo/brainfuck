module DigitalRoot where

import Brainfuck.CodeGen.C99
import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.Parser
import Brainfuck.Data.AST
import Brainfuck.Data.Brainfuck
import Brainfuck.Interpreter
import Brainfuck.Optimization.Assignment
import Brainfuck.Optimization.General
import Data.Foldable
import Data.Word

string :: String
string = ">,[<++++++[>--------<-]>[>+<-]>[<+>>+<-]>[<+>-]<<[-[-[-[-[-[-[-[-[-[[-]>---------<]]]]]]]]]],]++++++[>++++++++<-]++++++++++>.<."

parsed :: [Brainfuck]
parsed = case parseBrainfuck string of
  Right bf -> bf
  Left er  -> error $ show er

compiled = compile parsed
opt1 = optimizeSets compiled
opt2 = optimizeExpressions opt1
opt3 = moveShifts opt2
opt4 = removeFromEnd opt3
opt5 = cleanUp opt4
opt6 = reduceCopyLoops opt5
opt7 = optimizeExpressions opt6
opt8 = optimizeSets opt7

out = putStr . showC

exe :: String -> AST -> String
exe inp = map chrIntegral . toList . output . run (newState inp :: State Word8)
