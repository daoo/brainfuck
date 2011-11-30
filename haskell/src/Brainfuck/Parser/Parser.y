{
--
-- Copyright (c) 2011 Daniel Oom, see license.txt for more info.
--

module Brainfuck.Parser.Parser where

import Brainfuck.Parser.Brainfuck
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

Expr ::                  { [Brainfuck] }
     : Token             { [BFToken $1] }
     | Token Expr        { BFToken $1 : $2 }
     | '[' Expr ']'      { [BFLoop $2] }
     | '[' Expr ']' Expr { BFLoop $2 : $4 }

Token ::    { Token }
Token : '+' { Plus }
      | '-' { Minus }
      | '>' { ShiftRight }
      | '<' { ShiftLeft }
      | '.' { Output }
      | ',' { Input }
{
parseError :: [Char] -> a
parseError str = error $ "Parse error at \"" ++ str ++ "\""

parse :: String -> [Brainfuck]
parse = parser
}

-- vim: set ft=happy :
