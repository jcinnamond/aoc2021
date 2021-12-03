module Main (main) where

import Binary

-- this reverses the strings, but it shouldn't matter if we're just counting the number of zeros and ones
rotate :: [String] -> [String] -> [String]
rotate [] ys = ys
rotate (x : xs) [] = rotate xs $ expand x
rotate (x : xs) ys = rotate xs $ distribute x ys

expand :: String -> [String]
expand = fmap (: "")

distribute :: String -> [String] -> [String]
distribute x xs = uncurry (:) <$> zip x xs

gamma :: String -> Bit
gamma s =
    let zeros = length $ filter (== '0') s
        ones = length $ filter (== '1') s
     in if zeros > ones then Zero else One

invert :: Binary -> Binary
invert = fmap invert'
  where
    invert' Zero = One
    invert' One = Zero

solve :: [String] -> Int
solve xs = toInt g * toInt d
  where
    g = gamma <$> rotate xs []
    d = invert g

main :: IO ()
main = interact $ show . solve . lines

-- 00100
-- 11110
-- 10110
-- 10111
-- 10101
-- 01111
-- 00111
-- 11100
-- 10000
-- 11001
-- 00010
-- 01010

-- gamma = 10110 = 23
-- delta = 01001 = 9
-- (delta = not gamma)
