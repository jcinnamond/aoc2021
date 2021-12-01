module Main (main) where

tripples :: [Int] -> [Int]
tripples (x : y : z : rest) = (x + y + z) : tripples (y : (z : rest))
tripples _ = []

countLarger :: [Int] -> Int
countLarger xs = length $ filter larger $ zip xs (tail xs)
  where
    larger (x, y) = x < y

main :: IO ()
main = interact $ show . countLarger . tripples . fmap read . lines