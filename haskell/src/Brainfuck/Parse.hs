{-# LANGUAGE LambdaCase #-}
module Brainfuck.Parse (parseBrainfuck) where

import Brainfuck.Data.Brainfuck
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

program :: BS.ByteString -> (BS.ByteString, Brainfuck)
program bs = maybe (BS.empty, Nop) f (BS.uncons bs)
  where
    f (x, xs) = case x of
      '+' -> Token Plus       <$> program xs
      '-' -> Token Minus      <$> program xs
      '>' -> Token ShiftRight <$> program xs
      '<' -> Token ShiftLeft  <$> program xs
      '.' -> Token Output     <$> program xs
      ',' -> Token Input      <$> program xs

      '[' -> let (xs', inner) = program xs
              in Repeat inner <$> program xs'
      ']' -> (xs, Nop)

      _ -> program xs

parseBrainfuck :: BS.ByteString -> Brainfuck
parseBrainfuck = snd . program
