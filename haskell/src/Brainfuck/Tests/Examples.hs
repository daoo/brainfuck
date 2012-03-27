module Brainfuck.Tests.Example where

import Data.Char
import Data.Foldable (toList)
import Data.Sequence (index)

import Test.QuickCheck hiding (output)

import Brainfuck.CommandLine.Run
import Brainfuck.Compiler.IL
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser
import Brainfuck.Parser.Brainfuck

-- Prints "Hello World!\n"
bfHello = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

-- Prints "brainfuck\n"
bfPrintBrainFuck = ">++++[>++++++<-]>-[[<+++++>>+<-]>-]<<[<]>>>>--.<<<-.>>>-.<.<.>---.<<+++.>>>++.<<---.[>]<<."

bfRot13   = "-,+[-[>>++++[>++++++++<-]<+<-[>+>+>-[>>>]<[[>+<-]>>+>]<<<<<-]]>>>[-]+>--[-[<->+++[-]]]<[++++++++++++<[>-[>+>>]>[+[<+>-]>+>>]<<<<<-]>>[<+>-]>[-[-<<[-]>>]<<[<<->>-]>>]<<[<<+>>-]]<[-]<.[-]<-,+]"
bfReverse = ">,[>,]<[.<]"
 
-- Outputs in unary (with bangs (!))
bfASCIIValues = "++++[>++++++++<-],[[>+.-<-]>.<,]"

-- Prints the squars (as ASCII numbers)
bfSquares = "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

-- Goes to the 30000th cell
bf30000 = "++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>]+++++[>+++++++<<++>-]>.<<."

-- {{{ QuickCheck properties
propReverse :: String -> Property
propReverse s = notElem '\NUL' s ==> out == reverse s
  where
    s' = s ++ "\NUL" -- bfReverse stops on 0
    out = brainfuck bfReverse s'

propASCIIValues :: NonEmptyList Char -> Property
propASCIIValues (NonEmpty s) = notElem '\NUL' s ==> values == map ord s
  where
    s'     = s ++ "\NUL" -- bfASCIIValues stops on 0
    out    = brainfuck bfASCIIValues s'
    values = map length $ words out
-- }}}

-- {{{ Tests
testSquares :: Bool
testSquares = and $ zipWith (==) squares out'
  where
    squares = map (^2) [0..100]

    out = brainfuck bfSquares ""
    out' = map read $ lines out

testIO :: Bool
testIO = length l == 2 && let [a, b] = l in a == b
  where
    l       = lines out
    out     = brainfuck bfIO "\n\EOT"
    bfIO    = ">,>+++++++++,>+++++++++++[<++++++<++++++<+>>>-]<<.>.<<-.>.>.<<."

--testSize :: Bool
testSize = out
  where
    out = brainfuck bf30000 ""

programs :: Bool
programs = all f bf
  where
    state = newState ""

    f (p, o) = toList (output $ run p' state) == o
      where
        p' = compile $ parse p

    bf = [ ("+++++[>+++++[>+++++<-]<-]>>.", [125])
         , ("++++++++++[>+++++++<-]>++.", [72])
         , ("+++++[>>+++++<<-]>>.", [25]) ]

asd =
  [ ("+++++[>+++++[>+++++<-]<-]>>.", [125])
  , ("++++++++++[>+++++++<-]>++.", [72])
  , ("+++++[>>+++++<<-]>>.", [25])
  , (bfHello, []) ]
-- }}}

-- vim: set fdm=marker :
