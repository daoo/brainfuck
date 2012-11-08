module DigitalRoot where

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.Optimize
import Brainfuck.Compiler.Target.C99
import Brainfuck.Data.IL
import Brainfuck.Interpreter
import Brainfuck.Parser
import Data.Foldable (toList)
import Data.Word

string = ">,[<++++++[>--------<-]>[>+<-]>[<+>>+<-]>[<+>-]<<[-[-[-[-[-[-[-[-[-[[-]>---------<]]]]]]]]]],]++++++[>++++++++<-]++++++++++>.<."
parsed = case parseBrainfuck string of
  Right bf -> bf
  Left er  -> error $ show er

compiled = compile parsed
opt1 = optimizeSets compiled
opt2 = mapIL optimizeExpressions opt1
opt3 = moveShifts opt2
opt4 = removeFromEnd opt3
opt5 = cleanUp opt4
opt6 = reduceCopyLoops opt5
opt7 = mapIL optimizeExpressions opt6
opt8 = optimizeSets opt7

out = putStr . showC

exe :: String -> [IL] -> String
exe inp = map chrIntegral . toList . getOutput . run (newState inp :: State Word8)
