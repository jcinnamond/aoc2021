module Binary (Bit(..), Binary, toBinary, toInt) where

data Bit = Zero | One deriving (Show, Eq)
type Binary = [Bit]

toBinary :: String -> Binary
toBinary = fmap toBinary
  where
    toBinary '0' = Zero
    toBinary '1' = One
    toBinary _ = undefined

toInt :: Binary -> Int
toInt = toInt' 0 . reverse
  where
    toInt' _ [] = 0
    toInt' p (Zero : xs) = toInt' (succ p) xs
    toInt' p (One : xs) = 2 ^ p + toInt' (succ p) xs
