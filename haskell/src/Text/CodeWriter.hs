module Text.CodeWriter
  ( CodeWriter()
  , writeCode
  , writeCode1

  , decIndent
  , incIndent
  , lined
  , indented

  , surround
  , parentheses

  , separate
  , commaSeparate

  , char
  , int
  , line
  , string

  , newline
  , space
  ) where

import Control.Arrow
import Control.Monad.State
import Data.ByteString.Builder
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS

-- |Indentation with a cached builder.
type Indent = (Int, Builder)

noindent :: Indent
noindent = (0, mempty)

indentStep :: Builder
indentStep = char8 ' ' <> char8 ' '

findent :: (Int -> Int) -> Indent -> Indent
findent f (i, _) = let i' = f i in (i', indents i')
  where
    indents = mconcat . (`replicate` indentStep)

inc, dec :: Indent -> Indent
inc = findent (+1)
dec = findent (subtract 1)

type CodeWriter = State (Indent, Builder)

writeCode :: CodeWriter () -> Builder
writeCode m = snd $ execState m (noindent, mempty)

writeCode1 :: CodeWriter () -> String
writeCode1 = BS.unpack . toLazyByteString . writeCode

getIndent :: CodeWriter Builder
getIndent = gets (snd . fst)

modIndent :: (Indent -> Indent) -> CodeWriter ()
modIndent = modify . first

tell :: Builder -> CodeWriter ()
tell = modify . second . flip mappend

incIndent, decIndent :: CodeWriter ()
incIndent = modIndent inc
decIndent = modIndent dec

char :: Char -> CodeWriter ()
char = tell . char8

int :: Int -> CodeWriter ()
int = tell . intDec

string :: String -> CodeWriter ()
string = tell . string8

indent :: CodeWriter ()
indent = getIndent >>= tell

-- |Write a code writer with indentation and an new line at the end.
lined :: CodeWriter a -> CodeWriter a
lined m = do
  indent
  a <- m
  newline
  return a

indented :: CodeWriter () -> CodeWriter ()
indented m = incIndent >> m >> decIndent

line :: String -> CodeWriter ()
line = lined . string

surround :: Char -> Char -> Bool -> CodeWriter () -> CodeWriter ()
surround a b True inner  = char a >> inner >> char b
surround _ _ False inner = inner

separate :: CodeWriter () -> [CodeWriter ()] -> CodeWriter ()
separate _   []           = return ()
separate _   [a]          = a
separate sep (a:as@(_:_)) = a >> sep >> separate sep as

commaSeparate :: [CodeWriter ()] -> CodeWriter ()
commaSeparate = separate (string ", ")

newline, space :: CodeWriter ()
newline = char '\n'
space   = char ' '

parentheses :: CodeWriter () -> CodeWriter ()
parentheses = surround '(' ')' True
