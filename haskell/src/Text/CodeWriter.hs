{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
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

  , writeCode
  , writeCode1
  ) where

import Brainfuck.Utility
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Data.ByteString.Builder
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS

newtype CodeWriter a = CodeWriter { runCodeWriter :: String -> (a, String, Builder) }

instance Monad CodeWriter where
  return x = CodeWriter $ \ind -> (x, ind, mempty)

  h >>= f = CodeWriter $ \ind -> case (runCodeWriter h) ind of
    (a, ind', build) -> case (runCodeWriter $ f a) ind' of
      (b, ind'', build') -> (b, ind'', build `mappend` build')

  fail = error

instance MonadState String CodeWriter where
  get     = CodeWriter $ \ind -> (ind, ind, mempty)
  put new = CodeWriter $ \_ -> ((), new, mempty)
  state f = CodeWriter $ \ind -> case f ind of (a, ind') -> (a, ind', mempty)

instance MonadWriter Builder CodeWriter where
  writer (a, w) = CodeWriter $ \ind -> (a, ind, w)
  tell w        = CodeWriter $ \ind -> ((), ind, w)

  listen f = do
    x <- f
    return (x, mempty)

  pass f = CodeWriter $ \ind -> case (runCodeWriter f) ind of
    ((a, g), ind', build) -> (a, ind', g build)

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

writeCode :: CodeWriter () -> Builder
writeCode f = thrd $ runCodeWriter f ""

writeCode1 :: CodeWriter () -> String
writeCode1 = BS.unpack . toLazyByteString . writeCode
