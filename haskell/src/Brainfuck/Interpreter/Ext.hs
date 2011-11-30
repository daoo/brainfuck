module Brainfuck.Interpreter.Ext where

inc, dec :: (Num a) => a -> a
dec i = i - 1
inc i = i + 1

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

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs

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

thrd :: (a, b, c) -> c
thrd (_, _, c) = c

