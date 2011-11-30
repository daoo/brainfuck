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

Expr ::                  { [Expr] }
     : Token             { [Token $1] }
     | Token Expr        { Token $1 : $2 }
     | '[' Expr ']'      { [Loop $2] }
     | '[' Expr ']' Expr { Loop $2 : $4 }

Token ::    { Token }
Token : '+' { Plus }
      | '-' { Minus }
      | '>' { Next }
      | '<' { Previous }
      | '.' { Output }
      | ',' { Input }
{
data Token = Plus | Minus | Next | Previous | Input | Output
  deriving Show

data Expr = Loop [Expr]
          | Token Token
  deriving Show

parseError :: [Char] -> a
parseError str = error $ "Parse error at \"" ++ str ++ "\""

parse :: String -> [Expr]
parse = parser
}

-- vim: set ft=happy :
