module Interpreter where

import Debug.Trace
import Data.Char

brainfuckChars = "-+<>[].,"

interpret :: String -> String -> String
interpret str inp = map chr $ interpret' ("", str) (iterate id 0, iterate id 0) (map ord inp)

inc, dec :: Int -> Int
dec i = i - 1
inc i = i + 1

type Memory = ([Int], [Int])

interpret' :: (String, String) -> Memory -> [Int] -> [Int]
interpret' (_, []) _ _ = []
interpret' s m inp     =
  case s of
    (_,'>':_) -> interpret' (shiftL s) (shiftL m) inp
    (_,'<':_) -> interpret' (shiftL s) (shiftR m) inp
    (_,'+':_) -> interpret' (shiftL s) (modify inc m) inp
    (_,'-':_) -> interpret' (shiftL s) (modify dec m) inp
    (_,'.':_) -> current m : interpret' (shiftL s) m inp
    (_,',':_) -> interpret' (shiftL s) (modify (const $ head inp) m) (tail inp)
    (_,'[':_) -> interpret' (shiftL s) m inp
    (_,']':_) | current m == 0 -> interpret' (shiftL s) m inp
              | otherwise      -> interpret' (goBackTo '[' s) m inp

-- Goes back through the tuple list until specified element is found
goBackTo :: (Eq a) => a -> ([a], [a]) -> ([a], [a])
goBackTo e l = until (\(a:_, _) -> a == e) shiftR l

current :: (a, [b]) -> b
current (_, x:_) = x
current _        = error "empty list"

-- Apply f to the first element of the second array
modify :: (a -> a) -> ([a], [a]) -> ([a], [a])
modify f (as, b:bs) = (as, f b:bs)

-- Move first element of second array to the beginning of first array
shiftL :: ([a], [a]) -> ([a], [a])
shiftL (as, b:bs) = (b:as, bs)

-- Move first element of first array to the beginning of the second array
shiftR :: ([a], [a]) -> ([a], [a])
shiftR (a:as, bs) = (as, a:bs)

