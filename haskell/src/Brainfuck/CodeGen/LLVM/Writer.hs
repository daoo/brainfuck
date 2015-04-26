{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Brainfuck.CodeGen.LLVM.Writer
  ( LLVMWriter
  , runLLVM
  , newLabel

  , writeLabelLine
  , writeLoad
  , writeStore
  , writeGetElemPtr
  , writeVoidCall
  , writeCall
  , writeBranch
  , writeBranchIf
  , writeCmpNeq
  , writeAdd
  , writeMul
  ) where

import Brainfuck.CodeGen.LLVM.Internal
import Control.Exception
import Control.Monad.State
import Data.ByteString.Builder (Builder)
import Data.ByteString.Short (ShortByteString)
import Data.Monoid
import Text.CodeWriter
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BS

toShortByteString :: Builder -> ShortByteString
toShortByteString = BS.toShort . BL.toStrict . B.toLazyByteString

type LLVMWriter = StateT Int CodeWriter

runLLVM :: LLVMWriter a -> CodeWriter a
runLLVM = (`evalStateT` 0)

newUnique :: LLVMWriter Int
newUnique = do
  x <- get
  put (succ x)
  return x

newLabel :: LLVMWriter Label
newLabel = build <$> newUnique
  where
    build x = Label (toShortByteString $ B.char8 'l' <> B.intDec x)

newLocal :: Type -> LLVMWriter Local
newLocal t = build <$> newUnique
  where
    build x = Local t (toShortByteString $ B.char8 't' <> B.intDec x)

writeLabel :: Label -> CodeWriter ()
writeLabel (Label s) = shortByteString "label %" >> shortByteString s

writeLabelLine :: Label -> LLVMWriter ()
writeLabelLine (Label s) = lift $ lined $ shortByteString s >> char ':'

writeGlobal :: Global -> CodeWriter ()
writeGlobal (Global s) = char '@' >> shortByteString s

writeUntypedLocal :: Local -> CodeWriter ()
writeUntypedLocal l = char '%' >> shortByteString (localName l)

writeTypedLocal :: Local -> CodeWriter ()
writeTypedLocal l = writeType (localType l) >> space >> writeUntypedLocal l

writeType :: Type -> CodeWriter ()
writeType = \case
  TBit   -> string "i1"
  TMem   -> string "i8*"
  TCell  -> string "i8"
  TPtr t -> writeType t >> char '*'
  TVoid  -> string "void"

writeUntypedValue :: Value -> CodeWriter ()
writeUntypedValue = \case
  VLocal l -> writeUntypedLocal l
  VLit i   -> int i
  VVoid    -> string "void"

writeTypedValue :: Value -> CodeWriter ()
writeTypedValue v = do
  writeType (valueType v)
  space
  writeUntypedValue v

writeLoad :: Local -> LLVMWriter Local
writeLoad l = do
  tmp <- newLocal (unpoint $ localType l)
  lift $ lined $ do
    writeUntypedLocal tmp
    string " = load "
    writeTypedLocal l
  return tmp

writeStore :: Value -> Local -> LLVMWriter ()
writeStore v l = assert (TPtr (valueType v) == localType l) $
  lift $ lined $ do
    string "store "
    writeTypedValue v
    string ", "
    writeTypedLocal l

writeGetElemPtr :: Type -> Local -> [Value] -> LLVMWriter Local
writeGetElemPtr t l vs = do
  tmp <- newLocal t
  lift $ lined $ do
    writeUntypedLocal tmp
    string " = getelementptr "
    writeTypedLocal l
    string ", "
    commaSeparate (map writeTypedValue vs)
  return tmp

writeVoidCall :: Global -> [Value] -> LLVMWriter ()
writeVoidCall f args = lift $ lined $ do
  string "call void "
  writeGlobal f
  parentheses (commaSeparate (map writeTypedValue args))

writeCall :: Type -> Global -> [Value] -> LLVMWriter Local
writeCall t f args = do
  tmp <- newLocal t
  lift $ lined $ do
    writeUntypedLocal tmp
    string " = call "
    writeType t
    string " "
    writeGlobal f
    parentheses (commaSeparate (map writeTypedValue args))
  return tmp

writeBranch :: Label -> LLVMWriter ()
writeBranch label = lift $ lined $ string "br " >> writeLabel label

writeBranchIf :: Value -> Label -> Label -> LLVMWriter ()
writeBranchIf v true false = lift $ lined $ do
  string "br "
  writeTypedValue v
  string ", "
  writeLabel true
  string ", "
  writeLabel false

writeAdd, writeMul, writeCmpNeq :: Value -> Value -> LLVMWriter Local
writeAdd    = writeOp "add" TCell
writeMul    = writeOp "mul" TCell
writeCmpNeq = writeOp "icmp ne" TBit

writeOp :: ShortByteString -> Type -> Value -> Value -> LLVMWriter Local
writeOp op t a b = assert (valueType a == TCell && valueType b == TCell) $ do
  tmp <- newLocal t
  lift $ lined $ do
    writeUntypedLocal tmp
    string " = "
    shortByteString op
    string " "
    writeTypedValue a
    string ", "
    writeUntypedValue b
  return tmp
