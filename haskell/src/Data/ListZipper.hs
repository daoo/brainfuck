module Data.ListZipper where

data ListZipper a = ListZipper
  { left :: [a]
  , focus :: a
  , right :: [a] }
  deriving (Show, Eq)

move :: Int -> ListZipper a -> ListZipper a
move (-1) (ListZipper (x : xs) y zz) = ListZipper xs x (y : zz)
move   1  (ListZipper xs y (z : zz)) = ListZipper (y : xs) z zz
move   0  lz                         = lz
move   n  lz | n < 0                 = move (n + 1) (move (-1) lz)
             | otherwise             = move (n - 1) (move (1) lz)

peek :: Int -> ListZipper a -> a
peek 0 lz             = focus lz
peek n lz | n < 0     = left lz !! abs n
          | otherwise = right lz !! n

apply :: (a -> a) -> ListZipper a -> ListZipper a
apply f (ListZipper xs y zz) = ListZipper xs (f y) zz
