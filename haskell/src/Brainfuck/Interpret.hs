{-# LANGUAGE LambdaCase #-}
module Brainfuck.Interpret (run, run1) where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Utility
import Control.Applicative
import Control.Monad.State.Strict
import Data.Char
import Data.Foldable
import Data.ListZipper
import Data.Word
import qualified Data.Sequence as S

type Input  = [Word8]
type Output = S.Seq Word8
type Memory = ListZipper Word8

data Machine = Machine
  { minput :: Input
  , moutput :: Output
  , mmemory :: Memory
  }

input :: State Machine Word8
input = do
  m <- get
  put (m { minput = tail (minput m) })
  return (head (minput m))

output :: Word8 -> State Machine ()
output x = modify $ \m -> m { moutput = moutput m S.|> x }

set :: Int -> Word8 -> State Machine ()
set d x = modify $ \m -> m { mmemory = applyAt (const x) d (mmemory m) }

shift :: Int -> State Machine ()
shift d = modify $ \m -> m { mmemory = move d (mmemory m) }

expr :: Expr -> State Machine Word8
expr e = mmemory <$> get >>= \mem -> return $ eval' (`peek` mem) e
  where
    eval' f = fromIntegral . eval (fromIntegral . f)

newMemory :: Memory
newMemory = ListZipper zeros 0 zeros
  where zeros = repeat 0

run1 :: String -> Tarpit -> String
run1 inp = map (chr . fromIntegral) . toList . run (map (fromIntegral . ord) inp)

run :: Input -> Tarpit -> Output
run inp ast = moutput $ execState (go ast) (Machine inp S.empty newMemory)
  where
    go :: Tarpit -> State Machine ()
    go = \case
      Nop                  -> return ()
      Instruction fun next -> function fun >> go next
      Flow ctrl inner next -> flow inner ctrl >> go next

    function = \case
      Shift s    -> shift s
      Assign d e -> expr e >>= set d
      PutChar e  -> expr e >>= output
      GetChar d  -> input >>= set d

    flow inner = \case
      Forever -> forever (go inner)
      Never   -> return ()
      Once    -> go inner
      While e -> while (continue e) (go inner)
      If e    -> when' (continue e) (go inner)

    continue = ((/= (0 :: Word8)) <$>) . expr
