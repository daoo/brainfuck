{-# LANGUAGE LambdaCase #-}
module Brainfuck.Interpret (Machine(..), newMachine, newMemory, run) where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Control.Applicative hiding (empty)
import Control.Monad.State.Strict
import Data.Char
import Data.ListZipper
import Data.Sequence
import Data.Word
import Ext

data Machine = Machine
  { input :: [Word8]
  , output :: Seq Word8
  , memory :: ListZipper Word8
  } deriving (Show, Eq)

newMemory :: ListZipper Word8
newMemory = ListZipper zeros 0 zeros
  where zeros = repeat 0

newMachine :: String -> Machine
newMachine inp = Machine (map (fromIntegral . ord) inp) empty newMemory

fout :: (Seq Word8 -> Seq Word8) -> State Machine ()
fout f = modify $ \s -> s { output = f (output s) }

finp :: ([Word8] -> [Word8]) -> State Machine ()
finp f = modify $ \s -> s { input = f (input s) }

fmem :: (ListZipper Word8 -> ListZipper Word8) -> State Machine ()
fmem f = modify $ \s -> s { memory = f (memory s) }

run :: Machine -> AST -> Machine
run m ast = execState (go ast) m
  where
    go :: AST -> State Machine ()
    go = \case
      Nop                  -> return ()
      Instruction fun next -> function fun >> go next
      Flow ctrl inner next -> flow inner ctrl >> go next

    function = \case
      Shift s    -> fmem (move s)
      Assign d e -> eval' e >>= \x -> fmem (set x d)
      PutChar e  -> eval' e >>= \x -> fout (|> x)
      GetChar d  -> head <$> input <$> get >>= \x -> fmem (set x d) >> finp tail

    flow inner = \case
      Forever -> forever (go inner)
      Never   -> return ()
      Once    -> go inner
      While e -> while (continue e) (go inner)
      If e    -> when' (continue e) (go inner)

    continue e = do
      x <- eval' e
      return $ x /= (0 :: Word8)

    eval' :: Expr -> State Machine Word8
    eval' e = do
      Machine _ _ mem <- get
      return $ fromIntegral $ eval (fromIntegral . (`peek` mem)) e

    set = applyAt . const
