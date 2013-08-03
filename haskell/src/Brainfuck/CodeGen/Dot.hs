{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Dot
  ( writeDot
  ) where

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
writeDot code = do
  line "digraph code {"
  indentedM $ evalStateT (go code) 0
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
          If e    -> exprNode diamond (string "If") e n
          While e -> exprNode diamond (string "While") e n

        nextNode n inner
        nextNode n next

    nextNode n code' = do
      n' <- newId
      makeEdge n n'
      go code'

    exprNode outline l expr n = do
      makeNode outline l n
      newId >>= makeEdge n
      makeExpr expr
