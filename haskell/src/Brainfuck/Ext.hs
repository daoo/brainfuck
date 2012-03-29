module Brainfuck.Ext where

import Data.List

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

mapOffset :: (Integral b) => (a -> a) -> b -> [a] -> [a]
mapOffset f 0 (x : xs) = f x : xs
mapOffset f i (x : xs) = x : mapOffset f (i - 1) xs
mapOffset _ _ _        = error "Negative number"

current :: ([a], b) -> a
current = head . fst

offset :: (Integral a) => a -> ([b], [b]) -> b
offset i tup | i < 0     = (`genericIndex` (abs i - 1)) $ snd tup
             | otherwise = (`genericIndex` i) $ fst tup

-- Apply f to the first element of the second array
modify :: (a -> a) -> ([a], [a]) -> ([a], [a])
modify = mapFst . mapHead

modify' :: (Integral b) => (a -> a) -> b -> ([a], [a]) -> ([a], [a])
modify' f i tup | i == 0    = modify f tup
                | i < 0     = mapSnd (mapOffset f (abs i - 1)) tup
                | otherwise = mapFst (mapOffset f i) tup

times :: (a -> a) -> Int -> a -> a
times f i a | i < 0 = error "Negative number"
            | i == 0 = a
            | otherwise = times f (i - 1) (f a)
