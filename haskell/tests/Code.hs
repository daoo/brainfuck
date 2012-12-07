module Code where

import Brainfuck.CodeGen.Indented
import Brainfuck.Data.AST
import Brainfuck.Interpreter
import Brainfuck.Optimization.Analysis
import Data.ListZipper
import Data.Sequence
import Data.Word
import Test.QuickCheck hiding (output)

newtype PrettyAST = PrettyAST { getAst :: AST }

instance Arbitrary PrettyAST where
  arbitrary = fmap PrettyAST arbitrary

instance Show PrettyAST where
  show (PrettyAST ast) = showIndented ast

compareFull :: (Integral a) => Int -> State a -> State a -> Bool
compareFull i (State _ out1 m1) (State _ out2 m2) =
  cut i m1 == cut i m2 && out1 == out2

compareOutput :: (Integral a) => State a -> State a -> Bool
compareOutput s1 s2 = output s1 == output s2

testCode :: (State Word8 -> State Word8 -> Bool) -> AST -> AST -> Bool
testCode f ast ast' = f (run state ast) (run state ast')
  where
    state :: State Word8
    state = State [1..] empty newMemory

checkTransform :: (AST -> AST) -> AST -> Bool
checkTransform f ast = testCode (compareFull s) ast ast'
  where
    ast' = f ast

    s = let (xsMin, xsMax) = memorySize ast
            (ysMin, ysMax) = memorySize ast'
         in 1 + abs xsMin + abs xsMax + abs ysMin + abs ysMax
