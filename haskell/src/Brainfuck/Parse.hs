module Brainfuck.Parse (parseBrainfuck) where

import Brainfuck.Data.Brainfuck
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

parseBrainfuck :: BS.ByteString -> Brainfuck
parseBrainfuck = snd . go
  where
    go bs = case BS.uncons bs of
      Nothing      -> (BS.empty, Nop)
      Just (x, xs) -> case x of
        '+' -> Token Plus       <$> go xs
        '-' -> Token Minus      <$> go xs
        '>' -> Token ShiftRight <$> go xs
        '<' -> Token ShiftLeft  <$> go xs
        '.' -> Token Output     <$> go xs
        ',' -> Token Input      <$> go xs

        '[' -> let (xs', inner) = go xs
                in Repeat inner <$> go xs'
        ']' -> (xs, Nop)

        _ -> go xs
