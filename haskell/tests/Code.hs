module Code where

import Brainfuck.CodeGen.Indented as Indented
import Brainfuck.Data.AST
import Brainfuck.Interpret
import Brainfuck.Optimization.Analysis
import Data.Char
import Data.Foldable
import Data.ListZipper
import Data.Sequence hiding (replicate)
import Test.QuickCheck hiding (output)

printableChar :: Gen Char
printableChar = frequency
  [ (5, choose ('a', 'z'))
  , (5, choose ('A', 'Z'))
  , (5, choose ('0', '9'))
  , (4, return ' ')
  ]

printableString :: Gen String
printableString = sized $ \n ->
  choose (0,n) >>= (sequence . (`replicate` printableChar))

newtype PrettyAST = PrettyAST { getAst :: AST }

instance Arbitrary PrettyAST where
  arbitrary = fmap PrettyAST arbitrary

instance Show PrettyAST where
  show (PrettyAST ast) = Indented.showAST ast

compareState :: Int -> State -> State -> Bool
compareState i (State _ out1 m1) (State _ out2 m2) =
  cut i m1 == cut i m2 && out1 == out2

compareOutput :: State -> State -> Bool
compareOutput s1 s2 = output s1 == output s2

testCode :: (State -> State -> Bool) -> AST -> AST -> Bool
testCode f ast ast' = f (run1 ast) (run1 ast')

checkTransform :: (AST -> AST) -> AST -> Bool
checkTransform f ast = testCode (compareState s) ast ast'
  where
    ast' = f ast

    s = let (xsMin, xsMax) = memorySize ast
            (ysMin, ysMax) = memorySize ast'
         in 1 + abs xsMin + abs xsMax + abs ysMin + abs ysMax

run1 :: AST -> State
run1 = run (State [1..] empty newMemory)

run2 :: String -> AST -> String
run2 inp = map (chr . fromIntegral) . toList . output . run (newState inp)
