module Brainfuck.Compiler.Target.Haskell where

import Brainfuck.Data.IL
import Text.CodeWriter

showHaskell :: [IL] -> String
showHaskell ils = writeCode $ do
  line "import Brainfuck.Data.Expr"
  line "import Brainfuck.Data.IOMemory"
  line ""
  line "main :: IO ()"
  line "main = do"
  incIndent
  line $ "m <- newMemory " ++ show defaultMem
  line "program (setMemory m) (putMemory m) (getMemory m) (evalMemory m) (whileMemory m) (ifMemory m)"
  decIndent
  line ""
  line "program set put get eval while when = runMemory $ do"
  incIndent

  code ils

  where
    defaultMem :: Int
    defaultMem = 30001

    code :: [IL] -> CodeWriter
    code [] = return ()
    code (x : xs) = case x of
      While e ys -> block "while" e ys >> code xs
      If e ys    -> block "when" e ys >> code xs
      Set d e    -> line ("set " ++ show d ++ " $ " ++ show e) >> code xs
      PutChar e  -> line ("put $ " ++ show e) >> code xs
      GetChar d  -> line ("get " ++ show d) >> code xs
      Shift d    -> line ("shift (" ++ show d ++ ")") >> code xs

    block str e ys = do
      lineM $ do
        string str
        string " ("
        string $ show e
        string ") $ do"
      incIndent
      code ys
      decIndent
