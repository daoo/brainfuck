{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Haskell where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Text.CodeWriter

showAST :: AST -> String
showAST ast = writeCode $ do
  line "import Brainfuck.Data.Expr"
  line "import Brainfuck.Data.IOMemory"
  line ""
  line "main :: IO ()"
  line "main = do"
  indentedM $ do
    line "m <- newMemory 30001"
    line "program (setMemory m) (putMemory m) (getMemory m) (evalMemory m) (whileMemory m) (ifMemory m)"
  line ""
  line "program set put get eval while when = runMemory $ do"
  indentedM $ go ast

  where
    go = \case
      Nop                  -> return ()
      Instruction fun next -> function fun >> go next
      Flow ctrl inner next -> control ctrl >> indentedM (go inner) >> go next

    control = \case
      Forever -> block "while" (Const 1)
      While e -> block "while" e
      Once    -> block "when" (Const 1)
      Never   -> block "when" (Const 0)
      If e    -> block "when" e

    -- TODO: Do not use ++
    function = \case
      Set d e    -> line $ "set " ++ show d ++ " $ " ++ show e
      PutChar e  -> line $ "put $ " ++ show e
      GetChar d  -> line $ "get " ++ show d
      Shift d    -> line $ "shift (" ++ show d ++ ")"

    block str e = do
      lineM $ do
        string str
        string " ("
        string $ show e
        string ") $ do"
