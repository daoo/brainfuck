module Interpreter (interpret) where

import Debug.Trace
import Data.Char

brainfuckChars :: [Char]
brainfuckChars = "-+<>[].,"

interpret :: String -> String -> String
interpret str inp = map chr $ interpret' (("", str), (zeros, zeros), (map ord inp))
  where
    zeros = iterate id 0

inc, dec :: Int -> Int
dec i = i - 1
inc i = i + 1

type Memory = ([Int], [Int])
type Input  = [Int]
type Code   = (String, String)
type State  = (Code, Memory, Input)

interpret' :: State -> [Int]
interpret' ((_, []), _, _) = []
interpret' (s, m, inp)     = --trace (show (mapBoth (takeWhile (/= 0)) m)) $
  case s of
    (_,'>':_) -> interpret' (shiftL s, shiftL m, inp)
    (_,'<':_) -> interpret' (shiftL s, shiftR m, inp)
    (_,'+':_) -> interpret' (shiftL s, modify inc m, inp)
    (_,'-':_) -> interpret' (shiftL s, modify dec m, inp)
    (_,'.':_) -> current m : interpret' (shiftL s, m, inp)
    (_,',':_) -> case inp of
                   []     -> []
                   (x:xs) -> interpret' (shiftL s, modify (const x) m, xs)
    (_,'[':_) -> interpret' (shiftL s, m, inp)
    (_,']':_) -> if current m == 0
                   then interpret' (shiftL s, m, inp)
                   else interpret' (goBackTo '[' s, m, inp)

-- Goes back through the tuple list until specified element is found
goBackTo :: (Eq a) => a -> ([a], [a]) -> ([a], [a])
goBackTo e l = until ((== e) . head. fst) shiftR l

current :: (a, [b]) -> b
current (_, x:_) = x
current _        = error "empty list"

-- Apply f to the first element of the second array
modify :: (a -> a) -> ([a], [a]) -> ([a], [a])
modify f (as, b:bs) = (as, f b:bs)
modify _ x          = x

-- Move first element of second array to the beginning of first array
shiftL :: ([a], [a]) -> ([a], [a])
shiftL (as, b:bs) = (b:as, bs)
shiftL x          = x

-- Move first element of first array to the beginning of the second array
shiftR :: ([a], [a]) -> ([a], [a])
shiftR (a:as, bs) = (as, a:bs)
shiftR x          = x

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)
