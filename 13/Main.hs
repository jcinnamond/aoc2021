module Main (main) where

import Data.List (elemIndex, group, isPrefixOf, partition, sort)
import Data.Maybe (fromJust, fromMaybe)

type Point = (Int, Int)
data Fold = FoldX Int | FoldY Int deriving (Show)
data Dot = On | Off deriving (Show)
type Grid = [Point]

parse :: [String] -> (Grid, [Fold])
parse xs = (parseGrid part1, parseFolds part2)
 where
  part1 = take toBlankLine xs
  part2 = drop (toBlankLine + 1) xs
  toBlankLine = fromMaybe (length xs) $ elemIndex "" xs

parseGrid :: [String] -> Grid
parseGrid = fmap parsePoint

parsePoint :: String -> Point
parsePoint s = (x, y)
 where
  x = read $ take toComma s
  y = read $ drop (toComma + 1) s
  toComma = fromJust $ elemIndex ',' s

parseFolds :: [String] -> [Fold]
parseFolds = fmap parseFold
 where
  parseFold :: String -> Fold
  parseFold s
    | "fold along y=" `isPrefixOf` s = FoldY $ read $ drop 13 s
    | "fold along x=" `isPrefixOf` s = FoldX $ read $ drop 13 s
    | otherwise = undefined

foldAll :: Grid -> [Fold] -> Grid
foldAll = foldl fold

fold :: Grid -> Fold -> Grid
fold g (FoldX x) = foldX x g
fold g (FoldY y) = foldY y g

foldX :: Int -> Grid -> Grid
foldX f g = uniq $ beforeFold ++ folded
 where
  (beforeFold, afterFold) = partition (\(x, _) -> x < f) g
  folded = fmap foldPoint afterFold
  foldPoint (x, y) = (f - (x - f), y)

foldY :: Int -> Grid -> Grid
foldY f g = uniq $ aboveFold ++ folded
 where
  (aboveFold, belowFold) = partition (\(_, y) -> y < f) g
  folded = fmap foldPoint belowFold
  foldPoint (x, y) = (x, f - (y - f))

uniq :: (Eq a) => (Ord a) => [a] -> [a]
uniq = fmap head . group . sort

pp :: Grid -> String
pp g =
  "Length = " ++ show (length g) ++ "\n\n"
    ++ unlines (fmap (ppRow g) [0 .. maxY])
 where
  maxY = maximum $ map snd g

ppRow :: Grid -> Int -> String
ppRow g y = fmap (getPoint y) [0 .. maxX]
 where
  maxX = maximum $ map fst g
  getPoint y x = if (x, y) `elem` g then '#' else ' '

solve :: (Grid, [Fold]) -> String
solve (g, fs) = pp $ foldAll g fs

main :: IO ()
main = interact $ solve . parse . lines