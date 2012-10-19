module Text.CodeWriter
  ( CodeWriter()
  , decIndent
  , incIndent
  , line
  , lineM
  , indentedM
  , string
  , writeCode
  ) where

import Control.Monad.Writer
import Control.Monad.State

type CodeWriter = StateT Int (Writer String) ()

indent :: Int -> String
indent i = replicate (2 * i) ' '

line :: String -> CodeWriter
line str = do
  i <- get
  tell $ indent i
  tell str
  tell "\n"

lineM :: CodeWriter -> CodeWriter
lineM f = do
  i <- get
  tell $ indent i
  f
  tell "\n"

indentedM :: CodeWriter -> CodeWriter
indentedM f = incIndent >> f >> decIndent

incIndent :: CodeWriter
incIndent = modify (+1)

decIndent :: CodeWriter
decIndent = modify (subtract 1)

string :: String -> CodeWriter
string = tell

writeCode :: CodeWriter -> String
writeCode = execWriter . (`execStateT` 0)
