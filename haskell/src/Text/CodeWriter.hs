module Text.CodeWriter
  ( CodeWriter()
  , decIndent
  , incIndent
  , lineM
  , indentedM

  , char
  , int
  , line
  , newline
  , string
  , surround

  , runCodeWriter
  , writeCode
  ) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BS

type CodeWriter = StateT String (Writer Builder)

incIndent, decIndent :: CodeWriter ()
incIndent = modify ((:) ' ' . (:) ' ')
decIndent = modify (drop 2)

lineM, indentedM :: CodeWriter () -> CodeWriter ()
lineM m     = indent >> m >> newline
indentedM m = incIndent >> m >> decIndent

indent :: CodeWriter ()
indent = get >>= string

char :: Char -> CodeWriter ()
char = tell . charUtf8

int :: Int -> CodeWriter ()
int = tell . intDec

surround :: Char -> Char -> Bool -> CodeWriter () -> CodeWriter ()
surround a b True inner  = char a >> inner >> char b
surround _ _ False inner = inner

newline :: CodeWriter ()
newline = char '\n'

string :: String -> CodeWriter ()
string = tell . stringUtf8

line :: String -> CodeWriter ()
line = lineM . string

writeCode :: CodeWriter () -> String
writeCode = BS.unpack . toLazyByteString . runCodeWriter

runCodeWriter :: CodeWriter () -> Builder
runCodeWriter = execWriter . (`execStateT` "")
