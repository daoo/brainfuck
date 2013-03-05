{-# LANGUAGE LambdaCase #-}
module Brainfuck.CodeGen.Dot (showExpr, showAST) where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Control.Monad.State
import Text.CodeWriter

type DotState a = StateT Int CodeWriter a

newId :: DotState Int
newId = modify (+1) >> get

node :: Int -> String -> DotState ()
node n label = lift $ lineM $ do
  string (show n)
  string " [label=\""
  string label
  string "\"];"

edge :: Int -> Int -> String -> DotState ()
edge from to color = lift $ lineM $ do
  string (show from)
  string " -> "
  string (show to)
  string " [color=\""
  string color
  string "\"];"

showExpr :: Expr -> DotState ()
showExpr = \case
  Value v -> do
    n <- get
    node n (value v)

  OperateUnary op a -> do
    n <- get
    node n (unop op)
    next n a

  OperateBinary op a b -> do
    n <- get
    node n (binop op)
    next n a
    next n b

  where
    next n e = do
      ne <- newId
      edge n ne "black"
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
    go :: AST -> StateT Int CodeWriter ()
    go = \case
      Nop -> do
        n <- get
        node n "Nop"

      Instruction fun next -> do
        n <- get
        case fun of
          Set d e   -> exprNode n (showString "Set " $ show d) e
          Shift i   -> node n (showString "Shift " $ show i)
          PutChar e -> exprNode n "PutChar" e
          GetChar d -> node n (showString "GetChar " $ show d)

        nextNode n next "black"

      Flow ctrl inner next -> do
        n <- get
        case ctrl of
          Forever -> node n "Forever"
          Once    -> node n "Once"
          Never   -> node n "Never"
          If e    -> exprNode n "If" e
          While e -> exprNode n "While" e

        nextNode n inner "blue"
        nextNode n next "black"

    nextNode n ast' color = do
      n' <- newId
      edge n n' color
      go ast'

    exprNode n l e = do
      node n l
      ne <- newId
      edge n ne "red"
      showExpr e
