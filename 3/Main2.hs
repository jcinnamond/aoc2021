module Main (main) where

import Binary

count :: (Eq a) => a -> [a] -> Int
count y xs = length $ filter (== y) xs

type Match = Int -> Int -> Bit

matchScrubber :: Match
matchScrubber zeros ones = if zeros > ones then One else Zero

matchOxygen :: Match
matchOxygen zeros ones = if zeros > ones then Zero else One

filterList :: Match -> Int -> [Binary] -> Binary
filterList _ _ [b] = b
filterList f p bs = filterList f (succ p) matching
  where
    col = fmap (!! p) bs
    zeros = count Zero col
    ones = count One col
    mostCommon = f zeros ones
    matching = filter (\row -> row !! p == mostCommon) bs

filterOxygen :: [Binary] -> Binary
filterOxygen = filterList matchOxygen 0

filterScrubber :: [Binary] -> Binary
filterScrubber = filterList matchScrubber 0

solve :: [String] -> Int
solve xs = toInt o * toInt s
    where o = filterOxygen bs
          s = filterScrubber bs
          bs = toBinary <$> xs

main :: IO ()
main = interact $ show . solve . lines