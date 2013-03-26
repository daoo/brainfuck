module Brainfuck.Parse (parseBrainfuck) where

import Brainfuck.Data.Brainfuck
import Control.Applicative ((<$>))
import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Prim
import qualified Data.ByteString.Char8 as BS

program :: Parser [Brainfuck]
program = many bfOp

bfOp :: Parser Brainfuck
bfOp = bfToken <|> bfLoop

bfToken :: Parser Brainfuck
bfToken = (char '+' >> return (Token Plus))
      <|> (char '-' >> return (Token Minus))
      <|> (char '>' >> return (Token ShiftRight))
      <|> (char '<' >> return (Token ShiftLeft))
      <|> (char '.' >> return (Token Output))
      <|> (char ',' >> return (Token Input))

bfLoop :: Parser Brainfuck
bfLoop = Repeat <$> between (char '[') (char ']') program

filterInvalid :: BS.ByteString -> BS.ByteString
filterInvalid = BS.filter (\c -> c == '+' || c == '-' || c == '<' || c == '>' ||
  c == '.' || c == ',' || c == '[' || c == ']')

parseBrainfuck :: BS.ByteString -> Either ParseError [Brainfuck]
parseBrainfuck = parse program "brainfuck" . filterInvalid
