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
  then Just (o, map (\(Poke d _) -> d) $ filter posPoke ils)
  else Nothing
  where
    isCopyLoop = nLen == 1 && pLen >= 1 && len - pLen - 1 == 0
      where
        len  = length ils
        nLen = length $ filter negPoke ils
        pLen = length $ filter posPoke ils

    negPoke (Poke _ (-1)) = True
    negPoke _             = False

    posPoke (Poke _ (1)) = True
    posPoke _            = False

copyLoop _            = Nothing
