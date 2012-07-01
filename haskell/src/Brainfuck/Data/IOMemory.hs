module Brainfuck.Interpreter.IOMemory where

import Brainfuck.Compiler.Expr
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Data.Array.IO
import Data.Char

type IOMemory a = StateT Int IO a
type Memory = IOArray Int Int

newMemory :: Int -> IO Memory
newMemory i = newArray (0, i) 0

evalMemory :: Memory -> Expr -> IOMemory Int
evalMemory _ (Const i)   = return i
evalMemory m (Get d)     = get >>= (\p -> liftIO $ readArray m (p + d))
evalMemory m (Add e1 e2) = (+) <$> (evalMemory m e1) <*> (evalMemory m e2)
evalMemory m (Mul e1 e2) = (*) <$> (evalMemory m e1) <*> (evalMemory m e2)

setMemory :: Memory -> Int -> Expr -> IOMemory ()
setMemory m d e = do
  p <- get
  r <- evalMemory m e
  liftIO $ writeArray m (p + d) r

putMemory :: Memory -> Expr -> IOMemory ()
putMemory m e = do
  c <- chr <$> evalMemory m e
  liftIO $ putChar c

getMemory :: Memory -> Int -> IOMemory ()
getMemory m d = do
  p <- get
  c <- liftIO $ ord <$> getChar
  liftIO $ writeArray m (p + d) c

shift :: Int -> IOMemory ()
shift = modify . (+)

ifMemory :: Memory -> Expr -> IOMemory () -> IOMemory ()
ifMemory m e f = do
  i <- evalMemory m e
  when (i /= 0) f

whileMemory :: Memory -> Expr -> IOMemory () -> IOMemory ()
whileMemory m e f = do
  i <- evalMemory m e
  when (i /= 0) (f >> whileMemory m e f)

runMemory :: IOMemory () -> IO ()
runMemory f = evalStateT f 0
