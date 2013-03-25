module Tests.Programs where

import Brainfuck.Compile
import Brainfuck.Data.Tarpit
import Brainfuck.Parse

parseCompile :: String -> Tarpit
parseCompile = (\(Right bf) -> compile bf) . parseBrainfuck

-- Prints "Hello World!\n"
bfHello :: Tarpit
bfHello = parseCompile
  "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

-- Prints "brainfuck\n"
bfPrintBrainFuck :: Tarpit
bfPrintBrainFuck = parseCompile
  ">++++[>++++++<-]>-[[<+++++>>+<-]>-]<<[<]>>>>--.<<<-.>>>-.<.<.>---.<<+++.>>>++.<<---.[>]<<."

bfRot13 :: Tarpit
bfRot13 = parseCompile
  "-,+[-[>>++++[>++++++++<-]<+<-[>+>+>-[>>>]<[[>+<-]>>+>]<<<<<-]]>>>[-]+>--[-[<->+++[-]]]<[++++++++++++<[>-[>+>>]>[+[<+>-]>+>>]<<<<<-]>>[<+>-]>[-[-<<[-]>>]<<[<<->>-]>>]<<[<<+>>-]]<[-]<.[-]<-,+]"

bfReverse :: Tarpit
bfReverse = parseCompile
  ">,[>,]<[.<]"

-- Outputs in unary (with bangs (!))
bfASCIIValues :: Tarpit
bfASCIIValues = parseCompile
  "++++[>++++++++<-],[[>+.-<-]>.<,]"

-- Prints the squars (as ASCII numbers)
bfSquares :: Tarpit
bfSquares = parseCompile
  "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

-- Goes to the 30000th cell
bf30000 :: Tarpit
bf30000 = parseCompile
  "++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>]+++++[>+++++++<<++>-]>.<<."

-- Prints the fibonacci sequence
bfFib :: Tarpit
bfFib = parseCompile
  ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"

-- Prints "\n\EOT"
bfIO :: Tarpit
bfIO = parseCompile
  ">,>+++++++++,>+++++++++++[<++++++<++++++<+>>>-]<<.>.<<-.>.>.<<."
