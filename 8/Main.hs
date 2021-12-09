module Main (main) where

import Data.List (elemIndex)

output :: String -> String
output s =
    let idx = elemIndex '|' s
     in case idx of
            Just i -> drop (i + 2) s
            _ -> ""

solve :: [String] -> String
solve = show . length . filter easyDigits . concatMap (words . output)
  where
    easyDigits w =
        let l = length w
         in l == 2 || l == 3 || l == 4 || l == 7

main :: IO ()
main = interact $ solve . lines