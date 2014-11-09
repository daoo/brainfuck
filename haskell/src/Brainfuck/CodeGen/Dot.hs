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

box, ellipse, diamond :: String
box     = "shape=\"box\""
ellipse = "shape=\"ellipse\""
diamond = "shape=\"diamond\""

newId :: DotState Id
newId = modify (+1) >> get

writeNode :: String -> CodeWriter () -> Id -> DotState ()
writeNode outline label n = lift $ lined $ do
  int n
  string " [label=\""
  label
  string "\" "
  string outline
  string "];"

writeEdge :: Id -> Id -> DotState ()
writeEdge from to = lift $ lined $ do
  int from
  string " -> "
  int to
  string ";"

writeExpr :: Expr -> Id -> DotState ()
writeExpr e = writeNode ellipse (string (show e))

writeDot :: Tarpit -> CodeWriter ()
writeDot code = do
  line "digraph code {"
  indented $ (`evalStateT` 0) $ do
    n <- newId
    writeNode box (string "Root") n
    go n code
  line "}"

  where
    go :: Id -> Tarpit -> DotState ()
    go n = \case
      Nop -> do
        n' <- newId
        writeNode box (string "Nop") n'
        writeEdge n n'

      Instruction fun next -> do
        n' <- newId
        case fun of
          Assign  d e -> expr      box (string "Assign "  >> int d) n' e
          Shift   i   -> writeNode box (string "Shift "   >> int i) n'
          PutChar e   -> expr      box (string "PutChar"          ) n' e
          GetChar d   -> writeNode box (string "GetChar " >> int d) n'

        writeEdge n n'
        go n' next

      Flow ctrl inner next -> do
        n' <- newId
        case ctrl of
          If e    -> expr diamond (string "If"   ) n' e
          While e -> expr diamond (string "While") n' e

        writeEdge n n'
        go n' inner
        go n' next

    expr outline l n e = do
      writeNode outline l n
      n' <- newId
      writeExpr e n'
      writeEdge n n'
