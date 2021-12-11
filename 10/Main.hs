module Main (main) where

import Data.List (sort)
import Data.Maybe (isNothing, mapMaybe)

type Stack = [Char]

corrupt :: Stack -> String -> Maybe Char
corrupt _ [] = Nothing
corrupt s (c : cs)
    | opening c = corrupt (c : s) cs
    | closes c (head s) = corrupt (tail s) cs
    | otherwise = Just c

opening :: Char -> Bool
opening c = c `elem` "([{<"

closes :: Char -> Char -> Bool
closes ')' '(' = True
closes ']' '[' = True
closes '}' '{' = True
closes '>' '<' = True
closes _ _ = False

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = undefined

solve :: [String] -> String
solve = show . sum . fmap points . mapMaybe (corrupt [])

complete :: Stack -> String -> String
complete s [] = fmap invert s
complete s (c : cs)
    | opening c = complete (c : s) cs
    | closes c (head s) = complete (tail s) cs
    | otherwise = complete s cs

invert :: Char -> Char
invert '(' = ')'
invert '[' = ']'
invert '{' = '}'
invert '<' = '>'
invert _ = undefined

autocompleteScore :: String -> Int
autocompleteScore = foldl (\total s -> total * 5 + s) 0 . fmap scoreClosing

scoreClosing :: Char -> Int
scoreClosing ')' = 1
scoreClosing ']' = 2
scoreClosing '}' = 3
scoreClosing '>' = 4
scoreClosing _ = undefined

takeMiddle :: [Int] -> Int
takeMiddle xs = xs !! (length xs `div` 2)

solve2 :: [String] -> String
solve2 = show . takeMiddle . sort . fmap (autocompleteScore . complete []) . filter (isNothing . corrupt [])

main :: IO ()
main = interact $ solve2 . lines