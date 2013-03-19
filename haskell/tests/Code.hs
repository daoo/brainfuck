module Code where

import Brainfuck.CodeGen.Indented as Indented
import Brainfuck.Data.AST
import Brainfuck.Interpret
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

testCode :: AST -> AST -> Bool
testCode ast ast' = run [] ast == run [] ast'

checkTransform :: (AST -> AST) -> AST -> Bool
checkTransform f ast = testCode ast (f ast)
