module Main (main) where

type Horizontal = Int
type Depth = Int
type Aim = Int

type Position = (Int, Int, Int)

data Command
    = Forward Int
    | Down Int
    | Up Int

parseCommand :: String -> Command
parseCommand s = case words s of
    ["forward", x] -> Forward $ read x
    ["down", x] -> Down $ read x
    ["up", x] -> Up $ read x
    _ -> undefined

forward :: Int -> Position -> Position
forward x (h, d, a) = (h + x, d, a)

dive :: Int -> Position -> Position
dive x (h, d, a) = (h, d + x * a, a)

aimDown :: Int -> Position -> Position
aimDown x (h, d, a) = (h, d, a + x)

aimUp :: Int -> Position -> Position
aimUp x (h, d, a) = (h, d, a - x)

move :: Command -> Position -> Position
move (Forward x) = forward x . dive x
move (Down x) = aimDown x
move (Up x) = aimUp x

multiplyPos :: Position -> Int
multiplyPos (x, y, _) = x * y

solve1 :: [String] -> Position
solve1 = foldl (flip move) (0,0,0) . fmap parseCommand

main :: IO ()
main = interact $ show . multiplyPos . solve1 . lines

-- forward 5
-- down 5
-- forward 8
-- up 3
-- down 8
-- forward 2