module Brainfuck.Compiler.Target.LLVM where

import Brainfuck.Data.IL
import Data.Int
import Data.Word
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic

mMain :: CodeGenModule (Function (IO ()))
mMain = do
  putchar <- newNamedFunction ExternalLinkage "putchar" :: TFunction (Word8 -> IO Word32)
  createFunction ExternalLinkage $ do
    call putchar (valueOf 72)
    ret ()

mFib :: CodeGenModule (Function (Int32 -> IO Int32))
mFib = recursiveFunction $ \ rfib n -> n %< 2 ? (1, rfib (n-1) + rfib (n-2))

genLLVM :: IO ()
genLLVM = do
  initializeNativeTarget
  --m <- newModule
  --defineModule m mMain
  --writeBitcodeToFile "incr.bc" m
  --f <- simpleFunction mMain
  --f

  fib <- simpleFunction mFib
  fib 22 >>= print
