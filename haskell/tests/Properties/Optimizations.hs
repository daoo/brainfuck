module Properties.Optimizations where

propOptAssign :: Tarpit -> Bool
propOptAssign code = exec [] code
