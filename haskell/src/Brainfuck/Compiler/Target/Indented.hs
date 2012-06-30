module Brainfuck.Compiler.Target.Indented (showIndented) where

import Text.CodeWriter
import Brainfuck.Compiler.IL

showIndented :: [IL] -> String
showIndented = writeCode . go
  where
    go []                = return ()
    go (If e ys : xs)    = block "If" e ys >> go xs
    go (While e ys : xs) = block "While" e ys >> go xs
    go (x : xs)          = line (show x) >> go xs

    block str e ys = do
      lineM $ do
        string str
        string " "
        string $ show e
      incIndent
      go ys
      decIndent
