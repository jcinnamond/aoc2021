module Main (main) where

newtype Position = Pos (Int, Int) deriving Show

instance Semigroup Position where
    (<>) (Pos (x, y)) (Pos (x', y')) = Pos (x + x', y + y')
instance Monoid Position where
    mempty = Pos (0, 0)

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

move :: Position -> Command -> Position
move p (Forward x) = p <> Pos (x, 0)
move p (Down x) = p <> Pos (0, x)
move p (Up x) = p <> Pos (0, - x)

multiplyPos :: Position -> Int
multiplyPos (Pos (x, y)) = x * y

solve1 :: [String] -> Int
solve1 xs = multiplyPos $ foldl move mempty $ map parseCommand xs

main :: IO ()
main = interact $ show . solve1 . lines

-- forward 5
-- down 5
-- forward 8
-- up 3
-- down 8
-- forward 2