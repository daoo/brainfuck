module Code where

import Brainfuck.CodeGen.Indented
import Brainfuck.Data.Tarpit
import Brainfuck.Interpret
import Test.QuickCheck hiding (output)
import Text.CodeWriter

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

newtype PrettyTarpit = PrettyTarpit { getAst :: Tarpit }

instance Show PrettyTarpit where
  show (PrettyTarpit ast) = writeCode $ writeIndented ast

testCode :: Tarpit -> Tarpit -> Bool
testCode ast ast' = run [] ast == run [] ast'

checkTransform :: (Tarpit -> Tarpit) -> Tarpit -> Bool
checkTransform f ast = testCode ast (f ast)
