{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Dot (showExpr, showAST) where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr hiding (get)
import Control.Monad.State
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

makeNode :: Outline -> Id -> String -> DotState ()
makeNode outline n label = lift $ lineM $ do
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
  Return v -> do
    n <- get
    makeNode Ellipse n (value v)

  OperateUnary op a -> do
    n <- get
    makeNode Ellipse n (unop op)
    next n a

  OperateBinary op a b -> do
    n <- get
    makeNode Ellipse n (binop op)
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

    unop = \case
      Id     -> "Id"
      Negate -> "-"

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
      Nop -> do
        n <- get
        makeNode Box n "Nop"

      Instruction fun next -> do
        n <- get
        case fun of
          Set d e   -> exprNode Box n (showString "Set " $ show d) e
          Shift i   -> makeNode Box n (showString "Shift " $ show i)
          PutChar e -> exprNode Box n "PutChar" e
          GetChar d -> makeNode Box n (showString "GetChar " $ show d)

        nextNode n next

      Flow ctrl inner next -> do
        n <- get
        case ctrl of
          Forever -> makeNode Diamond n "Forever"
          Once    -> makeNode Diamond n "Once"
          Never   -> makeNode Diamond n "Never"
          If e    -> exprNode Diamond n "If" e
          While e -> exprNode Diamond n "While" e

        nextNode n inner
        nextNode n next

    nextNode n ast' = do
      n' <- newId
      makeEdge n n'
      go ast'

    exprNode outline n l expr = do
      makeNode outline n l
      ne <- newId
      makeEdge n ne
      showExpr expr
