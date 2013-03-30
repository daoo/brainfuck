{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Dot (writeDot) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Control.Monad.State.Strict
import Text.CodeWriter

type Id         = Int
type DotState a = StateT Id CodeWriter a

box, ellipse, diamond :: CodeWriter ()
box     = string "shape=\"box\""
ellipse = string "shape=\"ellipse\""
diamond = string "shape=\"diamond\""

newId :: DotState Id
newId = modify (+1) >> get

makeNode :: CodeWriter () -> CodeWriter () -> Id -> DotState ()
makeNode outline label n = lift $ lineM $ do
  int n
  string " [label=\""
  label
  string "\" "
  outline
  string "];"

makeEdge :: Id -> Id -> DotState ()
makeEdge from to = lift $ lineM $ do
  int from
  string " -> "
  int to
  string ";"

makeExpr :: Expr -> DotState ()
makeExpr e = get >>= makeNode ellipse (string (show e))

writeDot :: Tarpit -> CodeWriter ()
writeDot ast = do
  line "digraph ast {"
  indentedM $ evalStateT (go ast) 0
  line "}"

  where
    go :: Tarpit -> DotState ()
    go = \case
      Nop -> get >>= makeNode box (string "Nop")

      Instruction fun next -> do
        n <- get
        case fun of
          Assign d e -> exprNode box (string "Assign " >> int d) e n
          Shift i    -> makeNode box (string "Shift " >> int i) n
          PutChar e  -> exprNode box (string "PutChar") e n
          GetChar d  -> makeNode box (string "GetChar " >> int d) n

        nextNode n next

      Flow ctrl inner next -> do
        n <- get
        case ctrl of
          Forever -> makeNode diamond (string "Forever") n
          Once    -> makeNode diamond (string "Once") n
          Never   -> makeNode diamond (string "Never") n
          If e    -> exprNode diamond (string "If") e n
          While e -> exprNode diamond (string "While") e n

        nextNode n inner
        nextNode n next

    nextNode n ast' = do
      n' <- newId
      makeEdge n n'
      go ast'

    exprNode outline l expr n = do
      makeNode outline l n
      newId >>= makeEdge n
      makeExpr expr
