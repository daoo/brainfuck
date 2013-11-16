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
import Data.ByteString.Builder
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS

newtype CodeWriter a = CodeWriter { runCodeWriter :: String -> (a, String, Builder) }

instance Monad CodeWriter where
  {-# INLINE return #-}
  return x = CodeWriter $ \ind -> (x, ind, mempty)

  {-# INLINE (>>=) #-}
  m >>= f = CodeWriter $ \ind -> case runCodeWriter m ind of
    (a, ind', build) -> case runCodeWriter (f a) ind' of
      (b, ind'', build') -> (b, ind'', build <> build')

  {-# INLINE (>>) #-}
  m1 >> m2 = CodeWriter $ \ind -> case runCodeWriter m1 ind of
    (_, ind', build) -> case runCodeWriter m2 ind' of
      (b, ind'', build') -> (b, ind'', build <> build')

  fail = error

{-# INLINE getIndent #-}
getIndent :: CodeWriter String
getIndent = CodeWriter $ \ind -> (ind, ind, mempty)

{-# INLINE modIndent #-}
modIndent :: (String -> String) -> CodeWriter ()
modIndent f = CodeWriter $ \ind -> ((), f ind, mempty)

{-# INLINE tell #-}
tell :: Builder -> CodeWriter ()
tell w = CodeWriter $ \ind -> ((), ind, w)

{-# INLINE incIndent #-}
{-# INLINE decIndent #-}
incIndent, decIndent :: CodeWriter ()
incIndent = modIndent ((:) ' ' . (:) ' ')
decIndent = modIndent (drop 2)

{-# INLINE char #-}
char :: Char -> CodeWriter ()
char = tell . charUtf8

{-# INLINE int #-}
int :: Int -> CodeWriter ()
int = tell . intDec

{-# INLINE newline #-}
newline :: CodeWriter ()
newline = char '\n'

{-# INLINE string #-}
string :: String -> CodeWriter ()
string = tell . stringUtf8

{-# INLINE indent #-}
indent :: CodeWriter ()
indent = getIndent >>= string

lineM, indentedM :: CodeWriter () -> CodeWriter ()
lineM m     = indent >> m >> newline
indentedM m = incIndent >> m >> decIndent

line :: String -> CodeWriter ()
line = lineM . string

surround :: Char -> Char -> Bool -> CodeWriter () -> CodeWriter ()
surround a b True inner  = char a >> inner >> char b
surround _ _ False inner = inner

writeCode :: CodeWriter () -> Builder
writeCode = thrd . (`runCodeWriter` "")

writeCode1 :: CodeWriter () -> String
writeCode1 = BS.unpack . toLazyByteString . writeCode
