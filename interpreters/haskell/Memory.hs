module Memory where

import Data.Char
import Data.Word

type Cell   = Word8
type Memory = ([Cell], [Cell])
type Code   = (String, String)
type State  = (Code, Memory)

chrCell :: Cell -> Char
chrCell = chr . fromIntegral

ordCell :: Char -> Cell
ordCell = fromIntegral . ord

brainfuckChars :: [Char]
brainfuckChars = "-+<>[].,"

newState :: String -> State
newState str = (("", str'), (zeros, zeros)) 
  where
    str' = filter (flip elem brainfuckChars) str
    zeros = iterate id 0
