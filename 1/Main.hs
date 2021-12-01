module Main (main) where

countLarger :: [Int] -> Int
countLarger xs = length $ filter larger $ zip xs (tail xs)
  where
    larger (x, y) = x < y

main :: IO ()
main = interact $ show . countLarger . fmap read . lines
