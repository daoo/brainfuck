module Brainfuck.Ext where

-- Move first element of second array to the beginning of first array
shiftL :: ([a], [a]) -> ([a], [a])
shiftL (as, b:bs) = (b:as, bs)
shiftL x          = x

-- Move first element of first array to the beginning of the second array
shiftR :: ([a], [a]) -> ([a], [a])
shiftR (a:as, bs) = (as, a:bs)
shiftR x          = x

zipBoth :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipBoth f (a, b) (c, d) = (f a c, f b d)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

times :: (a -> a) -> Int -> a -> a
times f i a | i < 0     = error "Negative number"
            | i == 0    = a
            | otherwise = times f (i - 1) (f a)

whileModified :: (Eq a) => (a -> a) -> a -> a
whileModified f a | a == a'   = a'
                  | otherwise = whileModified f a'
  where a' = f a

mapIndex :: (Integral b) => (a -> a) -> b -> [a] -> [a]
mapIndex f 0 (x : xs) = f x : xs
mapIndex f i (x : xs) = x : mapIndex f (i - 1) xs
mapIndex _ _ _        = error "Index out of range"

