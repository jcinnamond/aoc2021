module Main (main) where

import Data.List (find)
import Data.Maybe (isNothing)

type Energy = Int
type Position = (Int, Int)
type Flashes = Int
type Grid = [[Energy]]

data Map = Map Grid Int Int Flashes deriving (Show)

parse :: String -> Map
parse s = Map (fmap parseLine rows) xlen ylen 0
 where
  rows = lines s
  xlen = length $ head rows
  ylen = length rows
  parseLine = fmap (read . (: []))

update :: Grid -> Position -> Energy -> Grid
update g (x, y) e = before ++ [updateRow r x e] ++ after
 where
  before = take y g
  after = drop (y + 1) g
  r = g !! y

updateRow :: [Energy] -> Int -> Energy -> [Energy]
updateRow r x e = left ++ [e] ++ right
 where
  left = take x r
  right = drop (x + 1) r

get :: Map -> Position -> Maybe Energy
get map@(Map m xlen ylen _) p@(x, y)
  | outside map p = Nothing
  | otherwise = Just $ (m !! y) !! x

around :: Position -> [Position]
around p@(x, y) = filter (/= p) $ [(x', y') | y' <- [y -1 .. y + 1], x' <- [x -1 .. x + 1]]

outside :: Map -> Position -> Bool
outside (Map _ xlen ylen _) (x, y) = x < 0 || x >= xlen || y < 0 || y >= ylen

step' :: Map -> Position -> Map
step' map@(Map g xlen ylen f) p = case energy of
  Nothing -> map
  Just 9 -> flash (newMap 10) $ around p
  Just e -> newMap (e + 1)
 where
  energy = get map p
  newMap e = Map (update g p e) xlen ylen f

flash :: Map -> [Position] -> Map
flash = foldl step'

walk :: Map -> (Map -> Position -> Map) -> Map
walk map@(Map _ xlen ylen _) fn = foldl fn map $ [(x, y) | y <- [0 .. ylen - 1], x <- [0 .. xlen -1]]

resetFlashed :: Map -> Map
resetFlashed map = walk map reset

reset :: Map -> Position -> Map
reset map@(Map g xlen ylen f) p = case get map p of
  Just e | e > 9 -> Map (update g p 0) xlen ylen (f + 1)
  _ -> map

step :: Map -> Map
step map = resetFlashed $ walk map step'

solve :: Int -> Map -> String
solve steps map = show $ foldl (\m _ -> step m) map [1 .. steps]

solve2 :: Int -> Map -> String
solve2 count map@(Map g _ _ _)
  | allFlash = show count
  | otherwise = solve2 (count + 1) $ step map
 where
  allFlash = isNothing (find (/= 0) (concat g))

main :: IO ()
main = interact $ solve2 0 . parse