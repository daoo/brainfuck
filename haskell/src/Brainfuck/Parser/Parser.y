{
--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Brainfuck.Parser.Parser where
}

%name parser
%tokentype { Char }
%error { parseError }

%token
  '+' { '+' }
  '-' { '-' }
  '>' { '>' }
  '<' { '<' }
  '.' { '.' }
  ',' { ',' }
  '[' { '[' }
  ']' { ']' }
%%

Expr ::             { Expr }
     : Token        { Token $1 }
     | Expr Expr    { Sequence $1 $2 }
     | Expr         { NoLoop $1 }
     | '[' Expr ']' { Loop $2 }

Token ::    { Token }
Token : '+' { Plus }
      | '-' { Minus }
      | '>' { Next }
      | '<' { Previous }
      | '.' { Output }
      | ',' { Input }
{
data Expr = Sequence Expr Expr
          | NoLoop Expr
          | Loop Expr
          | Token Token
  deriving Show

data Token = Plus | Minus | Next | Previous | Input | Output
  deriving Show

parseError :: [Char] -> a
parseError _ = error "Parse error"

parse :: String -> Expr
parse = parser
}
