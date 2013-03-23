{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Dot (showExpr, showAST) where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Control.Monad.State.Strict
import Text.CodeWriter

type Id = Int

type DotState a = StateT Id CodeWriter a

data Outline = Box | Ellipse | Diamond

instance Show Outline where
  show = \case
    Box     -> "shape=\"box\""
    Ellipse -> "shape=\"ellipse\""
    Diamond -> "shape=\"diamond\""

newId :: DotState Id
newId = modify (+1) >> get

makeNode :: Outline -> String -> Id -> DotState ()
makeNode outline label n = lift $ lineM $ do
  string (show n)
  string " [label=\""
  string label
  string "\" "
  string (show outline)
  string "];"

makeEdge :: Id -> Id -> DotState ()
makeEdge from to = lift $ lineM $ do
  string (show from)
  string " -> "
  string (show to)
  string ";"

showExpr :: Expr -> DotState ()
showExpr = \case
  Return v -> get >>= makeNode Ellipse (value v)

  OperateBinary op a b -> do
    n <- get
    makeNode Ellipse (binop op) n
    next n a
    next n b

  where
    next n e = do
      ne <- newId
      makeEdge n ne
      showExpr e

    value = \case
      Get d   -> showString "#" $ show d
      Const c -> show c

    binop = \case
      Add -> "+"
      Mul -> "*"

showAST :: AST -> String
showAST ast = writeCode $ do
  line "digraph ast {"
  indentedM $ evalStateT (go ast) 0
  line "}"

  where
    go :: AST -> DotState ()
    go = \case
      Nop -> get >>= makeNode Box "Nop"

      Instruction fun next -> do
        n <- get
        case fun of
          Assign d e -> exprNode Box (showString "Assign " $ show d) e n
          Shift i    -> makeNode Box (showString "Shift " $ show i) n
          PutChar e  -> exprNode Box "PutChar" e n
          GetChar d  -> makeNode Box (showString "GetChar " $ show d) n

        nextNode n next

      Flow ctrl inner next -> do
        n <- get
        case ctrl of
          Forever -> makeNode Diamond "Forever" n
          Once    -> makeNode Diamond "Once" n
          Never   -> makeNode Diamond "Never" n
          If e    -> exprNode Diamond "If" e n
          While e -> exprNode Diamond "While" e n

        nextNode n inner
        nextNode n next

    nextNode n ast' = do
      n' <- newId
      makeEdge n n'
      go ast'

    exprNode outline l expr n = do
      makeNode outline l n
      newId >>= makeEdge n
      showExpr expr
