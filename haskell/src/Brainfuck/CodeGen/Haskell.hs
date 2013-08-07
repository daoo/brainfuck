{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Haskell
  ( writeHaskell
  ) where

import Brainfuck.Data.Tarpit
import Text.CodeWriter

writeHaskell :: Tarpit -> CodeWriter ()
writeHaskell code = do
  line "import Brainfuck.Data.Expr"
  line "import Brainfuck.Data.IOMachine"
  line ""
  line "main :: IO ()"
  line "main = runMemory 30001 $ do"
  indentedM $ go code

  where
    go = \case
      Nop                  -> return ()
      Instruction fun next -> lineM (function fun) >> go next
      Flow ctrl inner next -> control ctrl >> indentedM (go inner) >> go next

    control = \case
      If e    -> block "when" e
      While e -> block "while" e

    function = \case
      Assign d e -> string "set "   >> safeint d       >> string " $ " >> string (show e)
      PutChar e  -> string "put $ " >> string (show e)
      GetChar d  -> string "get "   >> safeint d
      Shift d    -> string "shift " >> safeint d

    safeint d = surround '(' ')' (d < 0) (int d)

    block str e = lineM $ do
      string str
      string " ("
      string $ show e
      string ") $ do"

writeHaskell2 :: Tarpit -> String
writeHaskell2 code = go code
  where
    preamble =
      "import Brainfuck.Data.Expr\
      \\
      \main :: IO ()\
      \main = print $ "

    go = \case
      Nop                  -> ""
      Instruction fun next -> go next ++ function fun
      Flow ctrl inner next -> go next ++ control ctrl ++ " (" ++ go inner ++ ") "

    function = undefined
    control  = undefined
