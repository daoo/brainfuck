module Test where

import Data.Char

import Test.QuickCheck

import Interpreter

bfInterpreter :: String
bfInterpreter = ">>>+[[-]>>[-]++>+>+++++++[<++++>>++<-]++>>+>+>+++++[>++>++++++<<-]+>>>,<++[[>[->>]<[>>]<<-]<[<]<+>>[>]>[<+>-[[<+>-]>]<[[[-]<]++<-[<+++++++++>[<->-]>>]>>]]<<]<]<[[<]>[[>]>>[>>]+[<<]<[<]<+>>-]>[>]+[->>]<<<<[[<<]<[<]+<<[+>+<<-[>-->+<<-[>+<[>>+<<-]]]>[<+>-]<]++>>-->[>]>>[>>]]<<[>>+<[[<]<]>[[<<]<[<]+[-<+>>-[<<+>++>-[<->[<<+>>-]]]<[>+<-]>]>[>]>]>[>>]>>]<<[>>+>>+>>]<<[->>>>>>>>]<<[>.>>>>>>>]<<[>->>>>>]<<[>,>>>]<<[>+>]<<[+<<]<]"

bfHello :: String
bfHello = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

bfPrintBrainFuck :: String
bfPrintBrainFuck = ">++++[>++++++<-]>-[[<+++++>>+<-]>-]<<[<]>>>>--.<<<-.>>>-.<.<.>---.<<+++.>>>++.<<---.[>]<<."

bfRot13 :: String
bfRot13 = "-,+[-[>>++++[>++++++++<-]<+<-[>+>+>-[>>>]<[[>+<-]>>+>]<<<<<-]]>>>[-]+>--[-[<->+++[-]]]<[++++++++++++<[>-[>+>>]>[+[<+>-]>+>>]<<<<<-]>>[<+>-]>[-[-<<[-]>>]<<[<<->>-]>>]<<[<<+>>-]]<[-]<.[-]<-,+]"

bfReverse :: String
bfReverse = ">,[>,]<[.<]"

-- Outputs in unary (with bangs (!))
bfASCIIValues :: String
bfASCIIValues = "++++[>++++++++<-],[[>+.-<-]>.<,]"

propReverse :: String -> Bool
propReverse s = brainfuck bfReverse s' == reverse s'
  where s' = filter (/= '\NUL') s

propASCIIValues :: NonEmptyList Char -> Bool
propASCIIValues (NonEmpty s) = out == " " || values == map ord s'
  where
    s' = filter (/= '\NUL') s
    out = brainfuck bfASCIIValues s'

    values = map length $ words out
