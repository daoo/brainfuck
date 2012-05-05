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
     : Token             { [Token $1] }
     | Token Expr        { Token $1 : $2 }
     | '[' Expr ']'      { [Repeat $2] }
     | '[' Expr ']' Expr { Repeat $2 : $4 }

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
parse ""  = []
parse str = parser str'
  where
    str' = filter (flip elem valid) str
    valid = "+-><.,[]"
}

-- vim: set ft=happy :
