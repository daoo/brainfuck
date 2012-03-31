module Brainfuck.Compiler.Analyzer where

import Control.Monad

import Brainfuck.Compiler.IL
import Brainfuck.Ext

--memoryRequired :: [IL] ->
memoryRequired ils = undefined
  where
    helper []                           = Just (0, 0)
    helper (Loop loop : ils)            = helper loop `f` helper ils
    helper (Shift (ShiftLeft i) : ils)  = helper ils
    helper (Shift (ShiftRight i) : ils) = helper ils
    helper (_ : ils)                    = zipBoth f (0, 0) $ helper ils

    f = liftM2 (+)

