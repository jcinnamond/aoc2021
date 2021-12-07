module Simulate (simulate, simulate') where

type Fish = Int
type DaysLeft = Int
type FishCount = Int

simulate :: [Fish] -> DaysLeft -> (Int, [Fish])
simulate fish 0 = (length fish, fish)
simulate fish d = simulate (concat $ step <$> fish) (pred d)

step :: Fish -> [Fish]
step 0 = [6, 8]
step d = [d - 1]

newtype Counter a = Counter (a, Int) deriving (Show)
instance (Eq a) => Eq (Counter a) where
    (==) (Counter (x, _)) (Counter (y, _)) = x == y
instance (Ord a) => Ord (Counter a) where
    compare (Counter (x, _)) (Counter (y, _)) = compare x y
instance Semigroup (Counter a) where
    (<>) (Counter (_, c1)) (Counter (x, c2)) = Counter (x, c1 + c2)

data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show)

insertWith :: Ord a => (a -> a -> a) -> Tree a -> a -> Tree a
insertWith _ Empty node = Tree Empty node Empty
insertWith f (Tree l node r) x
    | x < node = Tree (insertWith f l x) node r
    | x > node = Tree l node (insertWith f r x)
    | otherwise = Tree l (f node x) r

insert :: Ord a => Tree a -> a -> Tree a
insert = insertWith const

step' :: Bucket -> Bucket -> Bucket
step' Empty b = b
step' (Tree Empty node@(Counter (0, c)) r) b = foldl (insertWith (<>)) (step' r b) [Counter (8, c), Counter (6, c)]
step' (Tree l node@(Counter (x, c)) r) b = insertWith (<>) (step' r (step' l b)) (Counter (x - 1, c))

type Bucket = Tree (Counter DaysLeft)
bucket :: [Fish] -> Bucket -> Bucket
bucket xs b = foldl (\b x -> insertWith (<>) b (Counter (x, 1))) b xs

incFish :: Counter DaysLeft -> Counter DaysLeft
incFish (Counter (x, c)) = Counter (x, c + 1)

count :: Bucket -> Int
count Empty = 0
count (Tree l (Counter (_, c)) r) = count l + c + count r

simulate' :: [Fish] -> DaysLeft -> (Int, Bucket)
simulate' fish = simulate'' (bucket fish Empty)

simulate'' :: Bucket -> DaysLeft -> (Int, Bucket)
simulate'' fish 0 = (count fish, fish)
simulate'' fish d = simulate'' (step' fish Empty) (pred d)
