module Line (
    Point (..),
    Line (..),
    Direction (..),
    VentMap (..),
    printMap,
    countMap,
    addVents,
    addVentsFiltering,
) where

data Point = Point Int Int deriving (Show, Eq)
instance Ord Point where
    compare (Point x1 y1) (Point x2 y2) = if y1 < y2 then LT else compare x1 x2

data Line = Line Point Point deriving (Show)
data Direction = Vertical | Horizontal | DiagonalUp | DiagonalDown deriving (Show, Eq)
type LineWithDirection = (Line, Direction)

withDirection :: Line -> LineWithDirection
withDirection l@(Line p1@(Point x1 y1) p2@(Point x2 y2))
    | y1 == y2 && x1 <= x2 = (l, Horizontal)
    | y1 == y2 && x1 > x2 = (Line p2 p1, Horizontal)
    | x1 == x2 && y1 <= y2 = (l, Vertical)
    | x1 == x2 && y1 > y2 = (Line p2 p1, Vertical)
    | x1 < x2 && y1 < y2 = (l, DiagonalDown)
    | x1 > x2 && y1 > y2 = (Line p2 p1, DiagonalDown)
    | x1 < x2 && y1 > y2 = (l, DiagonalUp)
    | x1 > x2 && y1 < y2 = (Line p2 p1, DiagonalUp)
    | otherwise = undefined

walk :: Line -> [Point]
walk l = walk' start d end
  where
    (Line start end, d) = withDirection l

walk' :: Point -> Direction -> Point -> [Point]
walk' start d end
    | start == end = [start]
    | otherwise = start : walk' (step start d) d end

step :: Point -> Direction -> Point
step (Point x y) Horizontal = Point (x + 1) y
step (Point x y) Vertical = Point x (y + 1)
step (Point x y) DiagonalUp = Point (x + 1) (y - 1)
step (Point x y) DiagonalDown = Point (x + 1) (y + 1)

data Tree a
    = TEmpty
    | TTree (Tree a) a (Tree a)
    deriving (Show)
type VentRow = Tree (Int, Int) -- xoffset, count
type VentMap = Tree (Int, VentRow) -- yoffset, row

addVents :: [Line] -> VentMap
addVents = addVentsFiltering (const True)

addVentsFiltering :: (Direction -> Bool) -> [Line] -> VentMap
addVentsFiltering p = addVentsFiltering' p TEmpty

addVentsFiltering' :: (Direction -> Bool) -> VentMap -> [Line] -> VentMap
addVentsFiltering' _ m [] = m
addVentsFiltering' pred m (l : ls) = addVentsFiltering' pred newMap ls
  where
    newMap = if pred d then addPoints else m
    addPoints = foldl addPoint m (walk l')
    (l', d) = withDirection l

addPoint :: VentMap -> Point -> VentMap
addPoint TEmpty (Point x y) = TTree TEmpty (y, addRow TEmpty x) TEmpty
addPoint (TTree l p'@(y', row) r) p@(Point x y)
    | y < y' = TTree (addPoint l p) p' r
    | y > y' = TTree l p' (addPoint r p)
    | otherwise = TTree l (y, addRow row x) r

addRow :: VentRow -> Int -> VentRow
addRow TEmpty x = TTree TEmpty (x, 1) TEmpty
addRow (TTree l p@(x', c) r) x
    | x < x' = TTree (addRow l x) p r
    | x > x' = TTree l p (addRow r x)
    | otherwise = TTree l (x, c + 1) r

countMap :: (Int -> Bool) -> VentMap -> Int
countMap _ TEmpty = 0
countMap pred (TTree l (_, row) r) = countMap pred l + countRow pred row + countMap pred r

countRow _ TEmpty = 0
countRow pred (TTree l (_, x) r) = countRow pred l + (if pred x then 1 else 0) + countRow pred r

count :: Tree a -> Int
count TEmpty = 0
count (TTree l _ r) = succ $ count l + count r

-- debug
printMap :: VentMap -> String
printMap p = printMap' p (Point 0 0) (Point 9 9)

maxPoint :: VentMap -> Point
maxPoint TEmpty = Point 0 0
maxPoint (TTree _ (y, row) TEmpty) = Point (maxRowPoint row) y
maxPoint (TTree _ _ r) = maxPoint r

maxRowPoint :: VentRow -> Int
maxRowPoint TEmpty = 0
maxRowPoint (TTree _ (x, _) TEmpty) = x
maxRowPoint (TTree _ _ r) = maxRowPoint r

printMap' :: VentMap -> Point -> Point -> String
printMap' m current@(Point x1 y1) end@(Point x2 y2)
    | current == end = "." --lookupPoint m current
    | x1 == x2 = lookupPoint m current ++ "\n" ++ printMap' m (Point 0 (y1 + 1)) end
    | otherwise = lookupPoint m current ++ printMap' m (Point (x1 + 1) y1) end

lookupPoint :: VentMap -> Point -> String
lookupPoint TEmpty _ = "."
lookupPoint (TTree l (y', row) r) p@(Point x y)
    | y < y' = lookupPoint l p
    | y > y' = lookupPoint r p
    | otherwise = lookupRow row x

lookupRow :: VentRow -> Int -> String
lookupRow TEmpty _ = "."
lookupRow (TTree l (x', c) r) x
    | x < x' = lookupRow l x
    | x > x' = lookupRow r x
    | otherwise = show c
