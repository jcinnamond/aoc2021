{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad.State
import Data.List (sort, sortBy)
import Data.Maybe (catMaybes)

type Position = (Int, Int)
type Row = [Int]
type Col = [Int]

neighbours :: Row -> Col -> Position -> [Int]
neighbours row col (x, y) = catMaybes [before row x, after row x, before col y, after col y]

before :: Row -> Int -> Maybe Int
before r 0 = Nothing
before r x = Just $ r !! (x - 1)

after :: Row -> Int -> Maybe Int
after r x
  | x >= length r - 1 = Nothing
  | otherwise = Just $ r !! (x + 1)

isLowPoint :: [[Int]] -> Position -> Bool
isLowPoint rows pos@(x, y) = val < minimum (neighbours row col pos)
 where
  val = row !! x
  row = rows !! y
  col = (!! x) <$> rows

findLowPoints :: [[Int]] -> Int -> Int -> Position -> [Int]
findLowPoints input rowLength colLength pos@(x, y)
  | x == rowLength && y == colLength - 1 = []
  | x == rowLength = findLowPoints input rowLength colLength (0, y + 1)
  | isLowPoint input pos = (input !! y) !! x : findLowPoints input rowLength colLength (x + 1, y)
  | otherwise = findLowPoints input rowLength colLength (x + 1, y)

parse :: [String] -> [[Int]]
parse xs = fmap (\c -> read [c] :: Int) <$> xs

solve :: [[Int]] -> String
solve input = show $ sum $ (+ 1) <$> findLowPoints input rowLength colLength (0, 0)
 where
  rowLength = length $ head input
  colLength = length input

type Map = [[Int]]
type Bounds = (Int, Int)

findBasins :: Map -> Bounds -> [Position] -> Position -> [Position]
findBasins map bounds visited p@(x, y)
  | p `elem` visited = visited
  | outside bounds p = visited
  | lookupPos map p == 9 = visited
  | otherwise = foldl (findBasins map bounds) (p : visited) (around p)

around :: (Num a1, Num a2) => (a1, a2) -> [(a1, a2)]
around (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

lookupPos :: Map -> Position -> Int
lookupPos m p@(x, y) = m !! y !! x

maybeLookupPos :: Map -> Bounds -> Position -> Maybe Int
maybeLookupPos m bounds p
  | outside bounds p = Nothing
  | otherwise = Just $ lookupPos m p

outside :: Bounds -> Position -> Bool
outside (maxX, maxY) (x, y) = x < 0 || x > maxX || y < 0 || y > maxY

findLowPos :: Map -> Bounds -> Position -> [Position]
findLowPos m bounds@(maxX, maxY) p@(x, y)
  | y > maxY = []
  | x > maxX = findLowPos m bounds (0, y + 1)
  | lookupPos m p < surrounding = p : next
  | otherwise = next
 where
  surrounding = minimum $ catMaybes $ maybeLookupPos m bounds <$> around p
  next = findLowPos m bounds (x + 1, y)

solve2 :: Map -> String
solve2 map = show $ product $ take 3 $ sortBy (flip compare) $ fmap length $ findBasins map bounds [] <$> findLowPos map bounds (0, 0)
 where
  bounds = (rowLength, colLength)
  rowLength = length (head map) - 1
  colLength = length map - 1

main :: IO ()
main = interact $ solve2 . parse . lines