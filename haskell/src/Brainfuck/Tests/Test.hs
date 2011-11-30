module Test where

import Data.Char

import Test.QuickCheck

import Brainfuck.CommandLine.Run
import Brainfuck.Interpreter.Interpreter

bfInterpreter = ">>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]"

bfHello = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
bfPrintBrainFuck = ">++++[>++++++<-]>-[[<+++++>>+<-]>-]<<[<]>>>>--.<<<-.>>>-.<.<.>---.<<+++.>>>++.<<---.[>]<<."

bfRot13 = "-,+[-[>>++++[>++++++++<-]<+<-[>+>+>-[>>>]<[[>+<-]>>+>]<<<<<-]]>>>[-]+>--[-[<->+++[-]]]<[++++++++++++<[>-[>+>>]>[+[<+>-]>+>>]<<<<<-]>>[<+>-]>[-[-<<[-]>>]<<[<<->>-]>>]<<[<<+>>-]]<[-]<.[-]<-,+]"
bfReverse = ">,[>,]<[.<]"
 
-- Outputs in unary (with bangs (!))
bfASCIIValues = "++++[>++++++++<-],[[>+.-<-]>.<,]"
bfSquares = "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

-- Tests
bfIO = ">,>+++++++++,>+++++++++++[<++++++<++++++<+>>>-]<<.>.<<-.>.>.<<."
bf30000 = "++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>]+++++[>+++++++<<++>-]>.<<."
bfObscure = "[]++++++++++[>>+>+>++++++[<<+<+++>>>-]<<<<-]\n\"A*$\";?@![#>>+<<]>[>>]<<<<[>++<[-]]>.>."

-- QuickCheck properties
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

testIO :: Bool
testIO = length l == 2 && l !! 0 == l !! 1
  where
    l       = lines out
    out     = brainfuck bfIO "\n\EOT"

--testSize :: Bool
testSize = out
  where
    out = brainfuck bf30000 ""

