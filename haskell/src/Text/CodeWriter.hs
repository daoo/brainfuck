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
  {-# INLINE return #-}
  return x = CodeWriter $ \ind -> (x, ind, mempty)

  {-# INLINE (>>=) #-}
  m >>= f = CodeWriter $ \ind -> case runCodeWriter m ind of
    (a, ind', build) -> case runCodeWriter (f a) ind' of
      (b, ind'', build') -> (b, ind'', build `mappend` build')

  {-# INLINE (>>) #-}
  m1 >> m2 = CodeWriter $ \ind -> case runCodeWriter m1 ind of
    (_, ind', build) -> case runCodeWriter m2 ind' of
      (b, ind'', build') -> (b, ind'', build `mappend` build')

  fail = error

instance MonadState String CodeWriter where
  {-# INLINE get #-}
  {-# INLINE put #-}
  {-# INLINE state #-}
  get     = CodeWriter $ \ind -> (ind, ind, mempty)
  put new = CodeWriter $ const ((), new, mempty)
  state f = CodeWriter $ \ind -> case f ind of (a, ind') -> (a, ind', mempty)

instance MonadWriter Builder CodeWriter where
  {-# INLINE writer #-}
  {-# INLINE tell #-}
  writer (a, w) = CodeWriter $ \ind -> (a, ind, w)
  tell w        = CodeWriter $ \ind -> ((), ind, w)

  {-# INLINE listen #-}
  listen f = do
    x <- f
    return (x, mempty)

  {-# INLINE pass #-}
  pass f = CodeWriter $ \ind -> case runCodeWriter f ind of
    ((a, g), ind', build) -> (a, ind', g build)

{-# INLINE incIndent #-}
{-# INLINE decIndent #-}
incIndent, decIndent :: CodeWriter ()
incIndent = modify ((:) ' ' . (:) ' ')
decIndent = modify (drop 2)

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

indent :: CodeWriter ()
indent = get >>= string

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
