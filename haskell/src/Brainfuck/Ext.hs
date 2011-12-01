module Brainfuck.Ext where

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

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs

current :: ([a], b) -> a
current = head . fst

-- Apply f to the first element of the second array
modify :: (a -> a) -> ([a], [a]) -> ([a], [a])
modify = mapFst . mapHead

times :: (a -> a) -> a -> Int -> a
times f a i | i < 0 = error "Negative number"
            | i == 0 = a
            | otherwise = times f (f a) (i - 1)
