{-# LANGUAGE PatternSynonyms #-}
module Brainfuck.Parse (parseBrainfuck) where

import Brainfuck.Data.Brainfuck
import Data.Bifunctor
import Data.ByteString (ByteString, uncons, empty)
import Data.Word (Word8)

parseBrainfuck :: ByteString -> Brainfuck
parseBrainfuck = fst . go
  where
    go bs = case uncons bs of
      Nothing      -> (Nop, empty)
      Just (x, xs) -> case x of
        TPlus       -> Token Plus       `first` go xs
        TMinus      -> Token Minus      `first` go xs
        TShiftRight -> Token ShiftRight `first` go xs
        TShiftLeft  -> Token ShiftLeft  `first` go xs
        TOutput     -> Token Output     `first` go xs
        TInput      -> Token Input      `first` go xs

        TRepeatStart -> uncurry first (bimap Repeat go (go xs))

        TRepeatEnd -> (Nop, xs)

        _ -> go xs

pattern TPlus, TMinus, TShiftRight, TShiftLeft, TOutput,
        TInput, TRepeatStart, TRepeatEnd :: Word8
pattern TPlus        = 43
pattern TMinus       = 45
pattern TShiftRight  = 62
pattern TShiftLeft   = 60
pattern TOutput      = 46
pattern TInput       = 44
pattern TRepeatStart = 91
pattern TRepeatEnd   = 93
