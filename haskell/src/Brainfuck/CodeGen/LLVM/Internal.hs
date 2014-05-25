{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.LLVM.Internal where

data Type = TBit | TCell | TMem | TPtr Type | TVoid
  deriving Eq

unpoint :: Type -> Type
unpoint (TPtr t) = t
unpoint _        = error "not a pointer type"

data Value
  = VLocal Local
  | VLit Int
  | VVoid

valueType :: Value -> Type
valueType = \case
  VLocal l -> localType l
  VLit   _ -> TCell
  VVoid    -> TVoid

newtype Label = Label String

newtype Global = Global String

data Local = Local { localType :: Type, localName :: String }
