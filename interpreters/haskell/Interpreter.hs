module Interpreter where

import Debug.Trace
import Data.Char

brainfuckChars = "-+<>[].,"

interpret :: String -> String -> String
interpret str inp = map chr $ interpret' ("", str) (iterate id 0, iterate id 0) (map ord inp)

inc, dec :: Int -> Int
dec i = i - 1
inc i = i + 1

interpret' :: (String, String) -> ([Int], [Int]) -> [Int] -> [Int]
interpret'   (_, [])   _          _               = []
interpret' s@(_,'>':_) m          inp             = interpret' (shiftL s) (shiftL m) inp
interpret' s@(_,'<':_) m          inp             = interpret' (shiftL s) (shiftR m) inp
interpret' s@(_,'+':_) m          inp             = interpret' (shiftL s) (modify inc m) inp
interpret' s@(_,'-':_) m          inp             = interpret' (shiftL s) (modify dec m) inp
interpret' s@(_,'.':_) m@(_, x:_) inp             = x : interpret' (shiftL s) m inp
interpret' s@(_,',':_) m          []              = error "Not enough input."
interpret' s@(_,',':_) m          (i:is)          = interpret' (shiftL s) (modify (const i) m) is
interpret' s@(_,'[':_) m          inp             = interpret' (shiftL s) m inp
interpret' s@(_,']':_) m@(_, x:_) inp | x == 0    = interpret' (shiftL s) m inp
                                      | otherwise = interpret' (goBackTo '[' s) m inp
interpret' s           m          inp             = interpret' (shiftL s) m inp

-- Goes back through the tuple list until specified element is found
goBackTo :: (Eq a) => a -> ([a], [a]) -> ([a], [a])
goBackTo e l = until (\(a:_, _) -> a == e) shiftR l

-- Apply f to the first element of the second array
modify :: (a -> a) -> ([a], [a]) -> ([a], [a])
modify f (as, b:bs) = (as, f b:bs)

-- Move first element of second array to the beginning of first array
shiftL :: ([a], [a]) -> ([a], [a])
shiftL (as, b:bs) = (b:as, bs)

-- Move first element of first array to the beginning of the second array
shiftR :: ([a], [a]) -> ([a], [a])
shiftR (a:as, bs) = (as, a:bs)

