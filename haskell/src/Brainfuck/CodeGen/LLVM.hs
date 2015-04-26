{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Brainfuck.CodeGen.LLVM (writeLLVM) where

import Brainfuck.CodeGen.LLVM.Internal
import Brainfuck.CodeGen.LLVM.Writer
import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Monad
import Data.ByteString (ByteString)
import Text.CodeWriter

runtime :: ByteString
runtime =
  "declare void @putchar(i8)\n\
  \declare i8 @getchar()\
  \\n\
  \@mem = internal global [30001 x i8] zeroinitializer\n"

writeLLVM :: Tarpit -> CodeWriter ()
writeLLVM code = do
  string runtime
  newline
  line "define i32 @main()"
  line "{"
  indented $ do
    unless (putConstOnly code) $ do
      line "%ptr = alloca i8*"
      line "%i0 = getelementptr [30001 x i8]* @mem, i32 0, i32 0"
      line "store i8* %i0, i8** %ptr"
    runLLVM $ writeInstrs code
    line "ret i32 0"
  line "}"

writeInstrs :: Tarpit -> LLVMWriter ()
writeInstrs = \case
  Nop                  -> return ()
  Instruction fun next -> function fun >> writeInstrs next
  Flow ctrl inner next -> control ctrl inner >> writeInstrs next

  where
    getchar = Global "getchar"
    putchar = Global "putchar"

    function = \case
      Shift s -> do
        t1 <- writeLoad ptr
        t2 <- writeGetElemPtr (TPtr TCell) t1 [VLit s]
        writeStore (VLocal t2) ptr

      GetChar d -> do
        v <- writeCall TCell getchar []
        writeStorePtr d (VLocal v)

      PutChar e -> do
        v <- writeExpr e
        writeVoidCall putchar [v]

      Assign d e -> do
        v <- writeExpr e
        writeStorePtr d v

    control ctrl inner = case ctrl of
      If e -> do
        true <- newLabel
        next <- newLabel
        v <- writeExpr e
        t <- writeCmpNeq v (VLit 0)
        writeBranchIf (VLocal t) true next
        writeLabelLine true
        writeInstrs inner
        writeBranch next
        writeLabelLine next

      While e -> do
        cond <- newLabel
        loop <- newLabel
        next <- newLabel
        writeBranch cond

        writeLabelLine cond
        v <- writeExpr e
        t <- writeCmpNeq v (VLit 0)
        writeBranchIf (VLocal t) loop next

        writeLabelLine loop
        writeInstrs inner
        writeBranch cond

        writeLabelLine next

writeExpr :: Expr -> LLVMWriter Value
writeExpr = deconsExpr variable constant
  where
    constant n = return $ VLit n

    variable 1 x Zero = VLocal `fmap` writeReadPtr x
    variable 1 x e    = do
      t1 <- writeReadPtr x
      t2 <- writeExpr e
      VLocal `fmap` writeAdd (VLocal t1) t2

    variable a x e = do
      t1 <- writeReadPtr x
      t2 <- writeMul (VLit a) (VLocal t1)
      t3 <- writeExpr e
      VLocal `fmap` writeAdd (VLocal t2) t3

tptr :: Type
tptr = TPtr (TPtr TCell)

ptr :: Local
ptr = Local tptr "ptr"

writePtr :: Int -> LLVMWriter Local
writePtr 0 = writeLoad ptr
writePtr d = do
  t1 <- writeLoad ptr
  t2 <- writeGetElemPtr (TPtr TCell) t1 [VLit d]
  return t2

writeReadPtr :: Int -> LLVMWriter Local
writeReadPtr d = do
  p <- writePtr d
  writeLoad p

writeStorePtr :: Int -> Value -> LLVMWriter ()
writeStorePtr d v = do
  p <- writePtr d
  writeStore v p
