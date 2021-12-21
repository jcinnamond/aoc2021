module Main (main) where

import Data.List (find)
import Data.Maybe (fromMaybe)

type Pair = (Char, Char)
type Rules = [(Pair, Char)]
type Rule = (Pair, Char)
type PairCount = [(Pair, Int)]

parse :: [String] -> (Char, Rules, PairCount)
parse s = (last $ head s, rules, pairs)
 where
  rules = parseRule <$> drop 2 s
  pairs = pairCount $ head s

parseRule :: String -> Rule
parseRule s = ((c1, c2), c3)
 where
  c1 = head s
  c2 = s !! 1
  c3 = s !! 6

pairCount :: String -> PairCount
pairCount s = foldr (updateCount 1) [] $ zip s (tail s)

rule :: Pair -> Rules -> Char
rule p rules = maybe undefined snd (find (\(p', c) -> p' == p) rules)

updateCount :: Int -> Pair -> PairCount -> PairCount
updateCount v k [] = [(k, v)]
updateCount v k (p@(p', c) : ps)
  | p' == k = (k, c + v) : ps
  | otherwise = p : updateCount v k ps

stepN :: Int -> Rules -> PairCount -> PairCount
stepN x r pc = foldl (\pc' _ -> step r pc') pc [1 .. x]

step :: Rules -> PairCount -> PairCount
step r = foldr (stepPair r) []

stepPair :: Rules -> (Pair, Int) -> PairCount -> PairCount
stepPair r (p, c) pc = updateCount c sndPair $ updateCount c fstPair pc
 where
  fstPair = (fst p, nc)
  sndPair = (nc, snd p)
  nc = rule p r

type ElementCount = [(Char, Int)]

incElemCount :: ElementCount -> Char -> Int -> ElementCount
incElemCount [] c v = [(c, v)]
incElemCount (e@(c', v') : xs) c v
  | c == c' = (c, v + v') : xs
  | otherwise = e : incElemCount xs c v

count :: Char -> PairCount -> ElementCount
count c = foldr inc [(c, 1)]
 where
  inc ((c, _), v) el = incElemCount el c v

diff :: ElementCount -> Int
diff ec = max - min
 where
  max = maximum counts
  min = minimum counts
  counts = fmap snd ec

solve :: (Char, Rules, PairCount) -> String
solve (lastChar, r, pc) = show $ diff $ count lastChar $ stepN 40 r pc

main :: IO ()
main = interact $ solve . parse . lines