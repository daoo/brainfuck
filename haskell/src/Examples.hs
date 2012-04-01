module Examples where

import Data.Char
import Data.Foldable (toList)
import Data.Word

import Test.QuickCheck hiding (output)

import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizer
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser
import Brainfuck.Run

-- Prints "Hello World!\n"
bfHello :: String
bfHello = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

-- Prints "brainfuck\n"
bfPrintBrainFuck :: String
bfPrintBrainFuck = ">++++[>++++++<-]>-[[<+++++>>+<-]>-]<<[<]>>>>--.<<<-.>>>-.<.<.>---.<<+++.>>>++.<<---.[>]<<."

bfRot13, bfReverse :: String
bfRot13   = "-,+[-[>>++++[>++++++++<-]<+<-[>+>+>-[>>>]<[[>+<-]>>+>]<<<<<-]]>>>[-]+>--[-[<->+++[-]]]<[++++++++++++<[>-[>+>>]>[+[<+>-]>+>>]<<<<<-]>>[<+>-]>[-[-<<[-]>>]<<[<<->>-]>>]<<[<<+>>-]]<[-]<.[-]<-,+]"
bfReverse = ">,[>,]<[.<]"
 
-- Outputs in unary (with bangs (!))
bfASCIIValues :: String
bfASCIIValues = "++++[>++++++++<-],[[>+.-<-]>.<,]"

-- Prints the squars (as ASCII numbers)
bfSquares :: String
bfSquares = "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

-- Goes to the 30000th cell
bf30000 :: String
bf30000 = "++++[>++++++<-]>[>+++++>+++++++<<-]>>++++<[[>[[>>+<<-]<]>>>-]>-[>+>+<<-]>]+++++[>+++++++<<++>-]>.<<."

bfFib :: String
bfFib = ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"

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
testSquares = and $ zipWith (==) squares out
  where
    squares :: [Integer]
    squares = map (^2) [0..100]

    out :: [Integer]
    out = map read $ lines $ brainfuck bfSquares ""

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

    f (p, o) = toList (output $ run state p') == o
      where
        p' = compile $ parse p

    bf :: [(String, [Word8])]
    bf = [ ("+++++[>+++++[>+++++<-]<-]>>.", [125])
         , ("++++++++++[>+++++++<-]>++.", [72])
         , ("+++++[>>+++++<<-]>>.", [25])
         , (bfHello, []) ]
-- }}}

compareOptimized :: String -> IO ()
compareOptimized bf = do
  putStrLn "Unoptimized:"
  putStrLn bf
  putStrLn "Optimized:"
  print $ decompile $ optimizeFully $ compile $ parse bf

-- vim: set fdm=marker :