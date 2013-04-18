module Brainfuck.Parse (parseBrainfuck) where

import Brainfuck.Data.Brainfuck
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

parse :: BS.ByteString -> (BS.ByteString, Brainfuck)
parse bs = maybe (BS.empty, Nop) f (BS.uncons bs)
  where
    f (x, xs) = case x of
      '+' -> Token Plus       <$> parse xs
      '-' -> Token Minus      <$> parse xs
      '>' -> Token ShiftRight <$> parse xs
      '<' -> Token ShiftLeft  <$> parse xs
      '.' -> Token Output     <$> parse xs
      ',' -> Token Input      <$> parse xs

      '[' -> let (xs', inner) = parse xs
              in Repeat inner <$> parse xs'
      ']' -> (xs, Nop)

      _ -> parse xs

parseBrainfuck :: BS.ByteString -> Brainfuck
parseBrainfuck = snd . parse
