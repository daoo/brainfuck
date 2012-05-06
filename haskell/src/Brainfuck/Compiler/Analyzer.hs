module Brainfuck.Compiler.Analyzer where

import Brainfuck.Compiler.IL

data Temp = TempLoop [Temp]
          | Delta Int
  deriving (Show)

memoryRequired :: [IL] -> [Temp]
memoryRequired = undefined

loopDepth :: IL -> Int
loopDepth (Loop _ ils) = (+1) $ maximum $ map loopDepth ils
loopDepth _            = 0

copyLoop :: IL -> Maybe (Int, [Int])
copyLoop (Loop _ [])  = Nothing
copyLoop (Loop o ils) = if isCopyLoop
  then Just (o, map (\(Add d _) -> d) $ filter posAdd ils)
  else Nothing
  where
    isCopyLoop = nLen == 1 && pLen >= 1 && len - pLen - 1 == 0
      where
        len  = length ils
        nLen = length $ filter negAdd ils
        pLen = length $ filter posAdd ils

    negAdd (Add _ (-1)) = True
    negAdd _            = False

    posAdd (Add _ (1)) = True
    posAdd _           = False

copyLoop _            = Nothing
