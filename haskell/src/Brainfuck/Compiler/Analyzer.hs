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

copyLoop :: IL -> Maybe (Int, [(Int, Int)])
copyLoop (Loop _ [])  = Nothing
copyLoop (Loop o ils) = if isCopyLoop
  then Just (o, map (\(Add d f) -> (d, f)) $ filter otherAdd ils)
  else Nothing
  where
    isCopyLoop = nLen == 1 && oLen >= 1 && len - oLen - 1 == 0
      where
        len  = length ils
        nLen = length $ filter negAdd ils
        oLen = length $ filter otherAdd ils

    negAdd (Add _ (-1)) = True
    negAdd _            = False

    otherAdd (Add _ n) = n /= (-1)
    otherAdd _         = False

copyLoop _            = Nothing
