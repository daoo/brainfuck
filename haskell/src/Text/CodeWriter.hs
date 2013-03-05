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

type CodeWriter = StateT Int (Writer String)

indent :: Int -> String
indent i = replicate (2 * i) ' '

string :: String -> CodeWriter ()
string = tell

line :: String -> CodeWriter ()
line = lineM . tell

lineM :: CodeWriter () -> CodeWriter ()
lineM cw = get >>= (tell . indent) >> cw >> tell "\n"

indentedM :: CodeWriter () -> CodeWriter ()
indentedM f = incIndent >> f >> decIndent

incIndent :: CodeWriter ()
incIndent = modify (+1)

decIndent :: CodeWriter ()
decIndent = modify (subtract 1)

writeCode :: CodeWriter () -> String
writeCode = execWriter . (`execStateT` 0)
