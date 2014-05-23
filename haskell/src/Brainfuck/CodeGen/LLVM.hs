{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.LLVM (writeLLVM) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Control.Monad
import Data.Char
import Text.CodeWriter

runtime :: String
runtime =
  "@declare void @putchar(i8)\n\
  \@declare i8 @getchar()\n\
  \\n\
  \%Tmem = type [i8 x 30001]\n"

memory :: String
memory =
  "%mem = alloca %Tmem\n\
  \%ptr = alloca %i8*\n\
  \%i0 = getelementptr %Tmem* %mem, 0, 0\n\
  \store %i8* %i0, %i8** %ptr\n"

writeLLVM :: Tarpit -> CodeWriter ()
writeLLVM code = do
  string runtime
  newline
  line "define i32 @main()"
  line "{"
  indented $ do
    unless (putConstOnly code) $ string memory
    writeInstrs code
    line "ret i32 0"
  line "}"

writeInstrs :: Tarpit -> CodeWriter ()
writeInstrs = \case
  Nop                  -> return ()
  Instruction fun next -> function fun >> writeInstrs next
  Flow ctrl inner next -> control ctrl inner >> writeInstrs next

  where
    ptr = VLocal (TPtr (TPtr TCell)) (Local "ptr")
    getchar = Global "getchar"
    putchar = Global "putchar"

    function = \case
      Shift s -> do
        p <- writeGetElemPtr ptr [VLit s]
        writeStore p ptr

      GetChar d -> do
        v <- writeCall getchar []
        writeToPtr "%t1"

      PutChar e -> do
        v <- writeExpr e
        writeVoidCall putchar [v]

      Assign d e -> do
        v <- writeExpr e
        p <- writeGetElemPtr
        writeStore v p

    control ctrl inner = case ctrl of
      If e -> do
        v <- writeExpr e
        writeBranchIf v true next
        writeLabel true
        writeStms inner
        writeBranch next
        writeLabel next

      While e -> do
        writeBranch cond
        writeLabel cond
        v <- writeExpr e
        writeBranchIf v loop next
        writeLabel loop
        writeStms inner
        writeBranch cond
        writeLabel next

data Type = TMem | TCell | TPtr Type | TVoid

data Value
  = VGlobal Type Global
  | VLocal Type Local
  | VLit Int

newtype Global = Global String
newtype Local  = Local String

writeVoidCall :: Global -> [Value] -> CodeWriter ()
writeVoidCall f args = do
  string "call void "
  writeGlobal f
  parentheses (commaSeparate (map writeValue args))
