{-# LANGUAGE LambdaCase #-}
module Brainfuck.Interpret
  ( run
  , exec
  , exec1
  , MachineState(..)
  , Input
  , Output
  ) where

import Brainfuck.Data.Tarpit
import Brainfuck.Data.ZipperMachine
import Data.Char
import Data.Foldable

exec :: Input -> Tarpit -> Output
exec inp = moutput . run inp

exec1 :: String -> Tarpit -> String
exec1 inp = map (chr . fromIntegral) . toList . exec (map (fromIntegral . ord) inp)

run :: Input -> Tarpit -> MachineState
run inp code = runMemory inp (go code)
  where
    go = \case
      Nop                  -> return ()
      Instruction fun next -> function fun >> go next
      Flow ctrl inner next -> flow inner ctrl >> go next

    function = \case
      Shift s    -> shift s
      Assign d e -> set d e
      PutChar e  -> put e
      GetChar d  -> get d

    flow inner = \case
      While e -> while e (go inner)
      If e    -> when e (go inner)
