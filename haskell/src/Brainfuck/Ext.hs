module Brainfuck.Ext where

onlyOne :: [a] -> Maybe a
onlyOne [a] = Just a
onlyOne _   = Nothing

-- |Move first element of second list to the beginning of first list
shiftL :: ([a], [a]) -> ([a], [a])
shiftL (as, b:bs) = (b:as, bs)
shiftL x          = x

-- |Move first element of first list to the beginning of the second list
shiftR :: ([a], [a]) -> ([a], [a])
shiftR (a:as, bs) = (as, a:bs)
shiftR x          = x

-- |Apply f to the first element of the tuple
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

-- |Apply f to the second element of the tuple
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

-- |Repeat a function a certain ammount of times
times :: (a -> a) -> Int -> a -> a
times f i a | i < 0     = error "Negative number"
            | i == 0    = a
            | otherwise = times f (i - 1) (f a)

-- |Repeat until a function returns the same function
whileModified :: (Eq a) => (a -> a) -> a -> a
whileModified f a | a == a'   = a'
                  | otherwise = whileModified f a'
  where a' = f a

-- |Apply a function to a specific element in a list
mapIndex :: (Integral b) => (a -> a) -> b -> [a] -> [a]
mapIndex f 0 (x : xs) = f x : xs
mapIndex f i (x : xs) = x : mapIndex f (i - 1) xs
mapIndex _ _ _        = error "Index out of range"

-- |Pipe a value through a list of functions
pipe :: [a -> a] -> a -> a
pipe xs x = foldl (\y f -> f y) x xs
