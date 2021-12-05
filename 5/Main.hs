module Main (main) where

import Line
import Parser
import System.Environment (getArgs)

horizontalOrVertical :: Direction -> Bool
horizontalOrVertical d = d == Horizontal || d == Vertical

solve1 :: [Line] -> String
solve1 = show . countMap (>= 2) . addVentsFiltering horizontalOrVertical

solve2 :: [Line] -> String
solve2 = show . countMap (>= 2) . addVents

runSolver :: ([Line] -> String) -> String -> String
runSolver solve inp = case parseInput inp of
    Left error -> show error
    Right lines -> solve lines

main :: IO ()
main = do
    args <- getArgs
    let solver = if args == ["part2"] then solve2 else solve1
    interact $ runSolver solver
