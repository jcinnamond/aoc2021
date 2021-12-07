module Main (main) where

nums :: String -> [Int]
nums "" = []
nums (',' : rest) = nums rest
nums s =
    let (v, rest) = break (== ',') s
     in read v : nums rest

simpleCost :: [Int] -> Int -> Int
simpleCost [] _ = 0
simpleCost (x : xs) y = abs (x - y) + simpleCost xs y

costs :: ([Int] -> Int -> Int) -> [Int] -> [Int]
costs cost xs = fmap (cost xs) [min .. max]
  where
    min = minimum xs
    max = maximum xs

solve :: [Int] -> Int
solve = minimum . costs simpleCost

sumCost :: [Int] -> Int -> Int
sumCost [] _ = 0
sumCost (x : xs) y = c (abs (x - y)) + sumCost xs y
  where
    c n = (n * (n - 1)) `div` 2 + n

solve2 :: [Int] -> Int
solve2 = minimum . costs sumCost

main :: IO ()
main = interact $ show . solve2 . nums