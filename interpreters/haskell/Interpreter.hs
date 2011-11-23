module Interpreter (brainfuck, brainfuckIO) where

import Debug.Trace

import Data.Word
import Data.Char

import Ext

brainfuckChars :: [Char]
brainfuckChars = "-+<>[].,"

brainfuck :: String -> String -> String
brainfuck str inp = map (chr . fromIntegral) $ interpret (newState str) inp'
  where
    str' = filter (flip elem brainfuckChars) str
    inp' = map ordCell $ inp ++ "\NUL"

brainfuckIO :: String -> IO ()
brainfuckIO str = interpretIO (newState str)

newState :: String -> State
newState str = (("", str'), (zeros, zeros)) 
  where
    str' = filter (flip elem brainfuckChars) str
    zeros = iterate id 0

type Cell   = Word8
type Memory = ([Cell], [Cell])
type Code   = (String, String)
type State  = (Code, Memory)

chrCell :: Cell -> Char
chrCell = chr . fromIntegral

ordCell :: Char -> Cell
ordCell = fromIntegral . ord

interpretIO :: State -> IO ()
interpretIO ((_, []), _)   = return ()
interpretIO state@(str, m) =
  case str of
    (_,'.':_) -> putChar (chrCell $ current m) >> interpretIO (shiftL str, m)
    (_,',':_) -> getChar >>= (\x -> interpretIO (shiftL str, modify (const $ ordCell x) m))
    _ -> interpretIO (nonIO state)

interpret :: State -> [Cell] -> [Cell]
interpret ((_, []), _) _     = []
interpret state@(str, m) inp = -- trace (show (mapBoth (takeWhile ( /= 0)) m)) $
  case str of
    (_,'.':_) -> current m : interpret (shiftL str, m) inp
    (_,',':_) -> case inp of
                   -- When we're out of input, just leave the memory alone
                   []     -> interpret state inp
                   (x:xs) -> interpret (shiftL str, modify (const x) m) xs
    _ -> interpret (nonIO state) inp

nonIO :: State -> State
nonIO (s, m) =
  case s of
    (_,'>':_) -> (shiftL s, shiftL m)
    (_,'<':_) -> (shiftL s, shiftR m)
    (_,'+':_) -> (shiftL s, modify inc m)
    (_,'-':_) -> (shiftL s, modify dec m)
    (_,'[':_) -> if current m == 0
                   then (skipPast ']' s, m)
                   else (shiftL s, m)
    (_,']':_) -> if current m == 0
                   then (shiftL s, m)
                   else (goBackTo '[' s, m)

-- Goes back through the tuple list until specified element is found
goBackTo :: (Eq a) => a -> ([a], [a]) -> ([a], [a])
goBackTo e = until ((== e) . head . fst) shiftR

skipPast :: (Eq a) => a -> ([a], [a]) -> ([a], [a])
skipPast e = until ((== e) . head . fst) shiftL

current :: (a, [b]) -> b
current (_, x:_) = x
current _        = error "empty list"

-- Apply f to the first element of the second array
modify :: (a -> a) -> ([a], [a]) -> ([a], [a])
modify = mapSnd . mapHead

