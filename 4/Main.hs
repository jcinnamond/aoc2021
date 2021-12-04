module Main (main) where

import System.Environment (getArgs)

type Square = (Int, Bool)
type Row = [Square]
type Board = [Row]

parseInput :: [String] -> ([Int], [Board])
parseInput xs = (parseDraws (head xs), parseBoards (tail xs))

parseDraws :: String -> [Int]
parseDraws "" = []
parseDraws (',' : rest) = parseDraws rest
parseDraws xs =
    let (v, rest) = break (== ',') xs
     in read v : parseDraws rest

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards ("" : rest) = parseBoards rest
parseBoards xs =
    let (b, rem) = parseBoard xs
     in b : parseBoards rem

parseBoard :: [String] -> (Board, [String])
parseBoard [] = ([], [])
parseBoard ("" : rest) = ([], rest)
parseBoard (row : rest) =
    let (b, rem) = parseBoard rest
     in (parseRow row : b, rem)

parseRow :: String -> Row
parseRow s = (\x -> (read x, False)) <$> words s

play :: [Int] -> [Board] -> (Int, Board)
play [] _ = (0, [])
play (draw : rest) boards =
    let boards' = mark draw boards
     in case check boards' of
            Just b -> (draw, b)
            Nothing -> play rest boards'

mark :: Int -> [Board] -> [Board]
mark x bs = markBoard x <$> bs

markBoard :: Int -> Board -> Board
markBoard x rows = markRow x <$> rows

markRow :: Int -> Row -> Row
markRow _ [] = []
markRow x (sq : sqs) = markSquare : markRow x sqs
  where
    markSquare = if v == x then (v, True) else sq
    v = fst sq

check :: [Board] -> Maybe Board
check [] = Nothing
check (b : bs) = if checkBoard b then Just b else check bs

checkBoard :: Board -> Bool
checkBoard b = checkRows b || checkCols b 0

checkRows :: Board -> Bool
checkRows = foldr ((||) . all snd) False

checkCols :: Board -> Int -> Bool
checkCols b c
    | c >= length b = False
    | otherwise = all (snd . (!! c)) b || checkCols b (succ c)

score :: Int -> Board -> Int
score _ [] = undefined
score x b = x * sum (unmatched b)

unmatched :: Board -> [Int]
unmatched b = concat (unmatchedRow <$> b)

unmatchedRow :: Row -> [Int]
unmatchedRow r = fst <$> filter (not . snd) r

solve1 :: [String] -> String
solve1 xs = show $ uncurry score $ uncurry play $ parseInput xs

----- part 2
playToLose :: [Int] -> [Board] -> (Int, Board)
-- if we run out of numbers then we're in trouble
playToLose [] _ = undefined
-- if we only have one board left, keep playing until it wins
playToLose (draw : rest) [b] =
    let b' = markBoard draw b
     in if checkBoard b' then (draw, b') else playToLose rest [b']
-- if we have many boards left then discard the winning ones and keep playing
playToLose (draw : rest) boards = playToLose rest $ discardWinning $ mark draw boards

discardWinning :: [Board] -> [Board]
discardWinning = filter (not . checkBoard)

solve2 :: [String] -> String
solve2 xs = show $ uncurry score $ uncurry playToLose $ parseInput xs

main :: IO ()
main = do
    args <- getArgs
    let solver = if args == ["part2"] then solve2 else solve1
    interact $ solver . lines