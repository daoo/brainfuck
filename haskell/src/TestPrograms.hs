module Main where

import Test.QuickCheck

import Data.Char
import Data.Foldable (toList)
import Data.Word

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Target.C99
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Brainfuck
import Brainfuck.Parser.Parser

-- {{{ Programs
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

-- Prints the fibonacci sequence
bfFib :: String
bfFib = ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]"

-- Prints "\n\EOT"
bfIO :: String
bfIO = ">,>+++++++++,>+++++++++++[<++++++<++++++<+>>>-]<<.>.<<-.>.>.<<."
-- }}}
-- {{{ QuickCheck properties
propReverse :: String -> Property
propReverse s = notElem '\NUL' s ==> out == reverse s
  where
    s'  = s ++ "\NUL" -- bfReverse stops on 0
    out = findOutput s' (compile $ parse bfReverse)

propASCIIValues :: NonEmptyList Char -> Property
propASCIIValues (NonEmpty s) = notElem '\NUL' s ==> values == map ord s
  where
    s'     = s ++ "\NUL" -- bfASCIIValues stops on 0
    out    = findOutput s' (compile $ parse bfASCIIValues)
    values = map length $ words out
-- {{{ Checkers
checkOutput :: String -> String -> [IL] -> Bool
checkOutput inp out ils = out == findOutput inp ils

data CheckRes  = Ok | UnOptFail | OptFail
data Checker = Checker
  { checkName :: String
  , checkFunc :: [IL] -> Bool
  , checkBf :: String }

check :: Checker -> CheckRes
check (Checker _ f bf) = case (f unopt, f opt) of
  (False, _)    -> UnOptFail
  (True, False) -> OptFail
  (True, True)  -> Ok
  where
    unopt = compile $ parse bf
    opt   = optimizeForC unopt
-- }}}
-- {{{ Main
handleResult :: Checker -> IO ()
handleResult c = case check c of
  Ok        -> return ()
  UnOptFail -> putStrLn $ "Unoptimized code failed for test: " ++ checkName c
  OptFail   -> putStrLn $ "Optimized code failed for test: " ++ checkName c

main :: IO ()
main = mapM_ handleResult
  [ Checker "30000" (checkOutput "" "#\n") bf30000
  ]
-- }}}
-- {{{ Helpers
parse :: String -> [Brainfuck]
parse str = case parseBrainfuck str of
  Left err -> error $ show err
  Right bf -> bf

prepareBf :: String -> ([IL], [IL])
prepareBf bf = (il, optimizeForC il)
  where il = compile $ parse bf

findOutput :: String -> [IL] -> String
findOutput inp = map chrIntegral . toList . getOutput . run state
  where
    state :: State Word8
    state = newState inp
-- }}}

-- vim: set fdm=marker :
