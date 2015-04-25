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
import Data.ByteString (ByteString)
import Data.Foldable
import qualified Data.ByteString as B

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \x y -> f (g x y)

infixr 9 .:

exec :: Input -> Tarpit -> Output
exec = moutput .: run

exec1 :: ByteString -> Tarpit -> ByteString
exec1 inp = (B.pack . map fromIntegral) . toList . exec ((map fromIntegral . B.unpack) inp)

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
