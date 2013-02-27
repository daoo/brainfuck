module Brainfuck.Parse (parseBrainfuck) where

import Brainfuck.Data.Brainfuck
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec

program :: Parser [Brainfuck]
program = many bfOp

bfOp :: Parser Brainfuck
bfOp = bfToken <|> bfLoop

bfToken :: Parser Brainfuck
bfToken = choice $ map (uncurry f)
  [ ('+', Plus)
  , ('-', Minus)
  , ('>', ShiftRight)
  , ('<', ShiftLeft)
  , ('.', Output)
  , (',', Input)
  ] where
    f c t = char c >> return (Token t)

bfLoop :: Parser Brainfuck
bfLoop = Repeat <$> between (char '[') (char ']') program

parseBrainfuck :: String -> Either ParseError [Brainfuck]
parseBrainfuck = parse program "brainfuck" . filter (`elem` "+-<>.,[]")
