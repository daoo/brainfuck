module Tests.Programs where

import Brainfuck.Compile
import Brainfuck.Data.Tarpit
import Brainfuck.Parse
import qualified Data.ByteString.Char8 as BS

parseCompile :: BS.ByteString -> Tarpit
parseCompile = compile . parseBrainfuck

-- Prints "Hello World!\n"
bfHello :: BS.ByteString
bfHello = BS.pack
  "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

-- Prints "brainfuck\n"
bfPrintBrainFuck :: BS.ByteString
bfPrintBrainFuck = BS.pack
  ">++++[>++++++<-]>-[[<+++++>>+<-]>-]<<[<]>>>>--.<<<-.>>>-.<.<.>---.<<+++.>>>++.<<---.[>]<<."

bfRot13 :: BS.ByteString
bfRot13 = BS.pack
  "-,+[-[>>++++[>++++++++<-]<+<-[>+>+>-[>>>]<[[>+<-]>>+>]<<<<<-]]>>>[-]+>--[-[<->+++[-]]]<[++++++++++++<[>-[>+>>]>[+[<+>-]>+>>]<<<<<-]>>[<+>-]>[-[-<<[-]>>]<<[<<->>-]>>]<<[<<+>>-]]<[-]<.[-]<-,+]"

bfReverse :: BS.ByteString
bfReverse = BS.pack
  ">,[>,]<[.<]"

-- Outputs in unary (with bangs (!))
bfASCIIValues :: BS.ByteString
bfASCIIValues = BS.pack
  "++++[>++++++++<-],[[>+.-<-]>.<,]"

-- Prints the squars (as ASCII numbers)
bfSquares :: BS.ByteString
bfSquares = BS.pack
  "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

-- Goes to the 30000th cell
bf30000 :: BS.ByteString
bf30000 = BS.pack
  "++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>]+++++[>+++++++<<++>-]>.<<."

-- Prints the fibonacci sequence
bfFib :: BS.ByteString
bfFib = BS.pack
  ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"

-- Prints "\n\EOT"
bfIO :: BS.ByteString
bfIO = BS.pack
  ">,>+++++++++,>+++++++++++[<++++++<++++++<+>>>-]<<.>.<<-.>.>.<<."
