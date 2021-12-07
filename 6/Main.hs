module Main (main) where

import Simulate
import System.Environment (getArgs)

nums :: String -> [Int]
nums "" = []
nums (',' : rest) = nums rest
nums s =
    let (v, rest) = break (== ',') s
     in read v : nums rest

solve1 :: Int -> [Int] -> String
solve1 d f = show d ++ show (simulate' f d)

solve2 :: [Int] -> String
solve2 f = show $ simulate f 256

main :: IO ()
main = do
    args <- getArgs
    -- let solver = if args == ["part2"] then solve2 else solve1
    inp <- nums <$> getContents
    mapM_ (\d -> print (solve1 (read d) inp)) args

-- let days = read $ head args
-- interact $ show . solve1 days . nums