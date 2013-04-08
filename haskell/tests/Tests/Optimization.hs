module Tests.Optimization where

import Brainfuck.CodeGen.Indented
import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Assignment
import Text.CodeWriter
import Utility

example1 :: Tarpit
example1 = Instruction (Assign 6 $ 0)
         $ Instruction (Assign 6 $ var 1 5 + var 1 6)
         $ Instruction (Assign 5 $ 0)
         $ Instruction (Assign 5 $ var 1 5 + var 1 7)
         $ Instruction (Assign 6 $ var 1 6 + var 1 7)
         $ Instruction (Assign 7 $ 0)
         $ Nop

test = tests [testMemory 100] example1 (optimizeAssign example1)

test2 = do
  putStrLn $ writeCode $ writeIndented example1
  putStrLn $ writeCode $ writeIndented $ optimizeAssign example1
