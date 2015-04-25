{-# LANGUAGE OverloadedStrings #-}
module Text.CodeWriter
  ( CodeWriter()
  , writeCode

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
  , shortByteString

  , newline
  , space
  ) where

import Control.Arrow
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Short (ShortByteString)
import Data.Monoid
import qualified Data.ByteString.Builder as B

-- |Indentation with a cached builder.
type Indent = (Int, Builder)

noindent :: Indent
noindent = (0, mempty)

indentStep :: Builder
indentStep = B.char8 ' ' <> B.char8 ' '

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
char = tell . B.char8

int :: Int -> CodeWriter ()
int = tell . B.intDec

string :: ByteString -> CodeWriter ()
string = tell . B.byteString

shortByteString :: ShortByteString -> CodeWriter ()
shortByteString = tell . B.shortByteString

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

line :: ByteString -> CodeWriter ()
line = lined . string

surround :: Char -> Char -> Bool -> CodeWriter () -> CodeWriter ()
surround a b True inner  = char a >> inner >> char b
surround _ _ False inner = inner

separate :: CodeWriter () -> [CodeWriter ()] -> CodeWriter ()
separate _   []           = return ()
separate _   [a]          = a
separate sep (a:as@(_:_)) = a >> sep >> separate sep as

commaSeparate :: [CodeWriter ()] -> CodeWriter ()
commaSeparate = separate (shortByteString ", ")

newline, space :: CodeWriter ()
newline = char '\n'
space   = char ' '

parentheses :: CodeWriter () -> CodeWriter ()
parentheses = surround '(' ')' True
