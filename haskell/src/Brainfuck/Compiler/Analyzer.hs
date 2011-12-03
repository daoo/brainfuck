module Brainfuck.Compiler.Analyzer where

import Brainfuck.Compiler.IL

memoryRequired :: [IL] -> Maybe Int
memoryRequired []                           = Just 0
memoryRequired ((Loop l):xs) | lShifts == 0 = memoryRequired xs
                             | otherwise    = Nothing
  where
    lShifts = memoryShifts l
memoryRequired ((LeftShifts i):xs)  = memoryRequired xs >>= (Just . (subtract i))
memoryRequired ((RightShifts i):xs) = memoryRequired xs >>= (Just . (+i))
memoryRequired (_:xs)               = memoryRequired xs

memoryShifts :: [IL] -> Int
memoryShifts []                   = 0
memoryShifts ((Loop l):xs)        = memoryShifts l + memoryShifts xs
memoryShifts ((RightShifts i):xs) = i + memoryShifts xs
memoryShifts ((LeftShifts i):xs)  = (-i) + memoryShifts xs
memoryShifts (_:xs)               = memoryShifts xs
