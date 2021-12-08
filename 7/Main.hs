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
  c n = (n * (n + 1)) `div` 2

lowestCost' ::
  -- | The list of positions to sum up
  [Int] ->
  -- | The position to move to
  Int ->
  -- | The current cost
  Int ->
  -- | The lowest cost found so far
  Maybe Int ->
  -- | The new lowest cost
  Int
lowestCost' [] _ acc _ = acc
lowestCost' (pos : ps) target acc lowest =
  let diff = abs (pos - target)
      cost = (diff * (diff + 1)) `div` 2
      newAcc = acc + cost
   in case lowest of
        Just l | newAcc > l -> l
        _ -> lowestCost' ps target newAcc lowest

fasterCost' :: [Int] -> [Int] -> Maybe Int -> Maybe Int
fasterCost' _ [] acc = acc
fasterCost' positions (t : ts) lowest =
  let l = lowestCost' positions t 0 lowest
   in fasterCost' positions ts (Just l)

fasterCost :: [Int] -> Maybe Int
fasterCost positions = fasterCost' positions [min .. max] Nothing
 where
  min = minimum positions
  max = maximum positions

main :: IO ()
main = interact $ show . fasterCost . nums