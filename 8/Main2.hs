module Main (main) where

import Data.List (elemIndex, find, sort)
import Data.Maybe (fromMaybe)

oneOf :: String -> String -> Bool
oneOf = occurs 1

bothOf :: String -> String -> Bool
bothOf = occurs 2

occurs :: Int -> String -> String -> Bool
occurs c s x = length (filter (`elem` s) x) == c

decode5 :: (String, String) -> String -> Char
decode5 (cf, bd) s
    | oneOf cf s && oneOf bd s = '2'
    | bothOf cf s && oneOf bd s = '3'
    | oneOf cf s && bothOf bd s = '5'
    | otherwise = undefined

decode6 :: (String, String) -> String -> Char
decode6 (cf, bd) s
    | bothOf cf s && oneOf bd s = '0'
    | oneOf cf s && bothOf bd s = '6'
    | bothOf cf s && bothOf bd s = '9'
    | otherwise = undefined

solve' :: (String, String) -> String -> Char
solve' known s = case length s of
    2 -> '1'
    3 -> '7'
    4 -> '4'
    5 -> decode5 known s
    6 -> decode6 known s
    7 -> '8'
    _ -> undefined

split :: String -> ([String], [String])
split s =
    let (digits, output) = break (== "|") $ words s
     in (digits, tail output)

findKnown :: Maybe String -> Maybe String -> (String, String)
findKnown (Just one) (Just four) = (one, difference one four)
  where
    difference a = filter (`notElem` a)
findKnown _ _ = undefined

solve :: String -> Int
solve s = read $ solve' known <$> outputs
  where
    (entries, outputs) = split s
    one = find ((== 2) . length) entries
    four = find ((== 4) . length) entries
    known = findKnown one four

solveAll :: String -> Int
solveAll s = sum $ solve <$> lines s

main :: IO ()
main = interact $ show . solveAll