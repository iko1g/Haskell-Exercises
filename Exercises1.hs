module Exercises1 (histogram) where

-- Exercise A1
ranges :: (Ord b, Num b, Enum b) => b -> b -> [(b, b)]
ranges lim n = takeWhile (\(x,y) -> x < lim) [(k*n, (k+1)*n - 1) | k <- [0..]]

count :: Ord a => (a, a) -> [a] -> Int
count (l,r) = length.filter (\x -> l <= x && x <= r)

histogram :: Int -> [Int] -> [Int]
histogram n xs = count <$> range <*> pure xs where range =(ranges (maximum xs) n)

-- Exercise A2

module Exercises1 (approxPi) where

fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

doublefac :: (Integral a) => a -> a
doublefac 0 = 1
doublefac 1 = 1
doublefac n = n * doublefac (n-2)

approxPi :: Int -> Double
approxPi n
|approxPi n == -1 = "error"
|approxPi n == -0 = 1
    where
        approximate = 2(fac n div doublefac 2*n+1) + approxPi(n-1)

-- Exercise A3
module Exercises1 (amSplit) where

amSplit :: Ord a => [a] -> [[a]]
amSplit xs = foldr f [] xs
    where
        f a [] = [[a]]
        f a xs'@(y:ys) | a <= head y = (a:y):ys
            |otherwise = [a]:xs'
