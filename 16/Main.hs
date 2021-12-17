module Main (main) where

import Data.Char (digitToInt, ord)
import Data.List (find)
import Data.Maybe (isNothing)

data Bit = Z | I deriving (Show, Eq)

hexToBits :: String -> [Bit]
hexToBits = concatMap charToBits

charToBits :: Char -> [Bit]
charToBits '0' = [Z, Z, Z, Z]
charToBits '1' = [Z, Z, Z, I]
charToBits '2' = [Z, Z, I, Z]
charToBits '3' = [Z, Z, I, I]
charToBits '4' = [Z, I, Z, Z]
charToBits '5' = [Z, I, Z, I]
charToBits '6' = [Z, I, I, Z]
charToBits '7' = [Z, I, I, I]
charToBits '8' = [I, Z, Z, Z]
charToBits '9' = [I, Z, Z, I]
charToBits 'A' = [I, Z, I, Z]
charToBits 'B' = [I, Z, I, I]
charToBits 'C' = [I, I, Z, Z]
charToBits 'D' = [I, I, Z, I]
charToBits 'E' = [I, I, I, Z]
charToBits 'F' = [I, I, I, I]
charToBits _ = undefined

type Version = Int

toInt :: [Bit] -> Int
toInt = toInt' 0 . reverse
  where
    toInt' _ [] = 0
    toInt' p (Z : xs) = toInt' (succ p) xs
    toInt' p (I : xs) = 2 ^ p + toInt' (succ p) xs

parseVersion :: [Bit] -> (Version, [Bit])
parseVersion bs = (toInt (take 3 bs), drop 3 bs)

data OpType = OpSum | OpProduct | OpMin | OpMax | OpGT | OpLT | OpEQ deriving (Show)
data PacketType = PData | POperator OpType deriving (Show)

parseType :: [Bit] -> (PacketType, [Bit])
parseType bs = (ptype, drop 3 bs)
  where
    x = toInt (take 3 bs)
    ptype = case x of
        0 -> POperator OpSum
        1 -> POperator OpProduct
        2 -> POperator OpMin
        3 -> POperator OpMax
        4 -> PData
        5 -> POperator OpGT
        6 -> POperator OpLT
        _ -> POperator OpEQ

parseDataPacket :: Version -> [Bit] -> (Packet, [Bit])
parseDataPacket v bs =
    let (d, rest) = parseDataPacket' bs
     in (DPacket v d, rest)

parseDataPacket' :: [Bit] -> ([Bit], [Bit])
parseDataPacket' (Z : bs) = splitAt 4 bs
parseDataPacket' (I : bs) =
    let (d, rest) = splitAt 4 bs
        (d', rest') = parseDataPacket' rest
     in (d ++ d', rest')
parseDataPacket' _ = undefined

data Packet = DPacket Version [Bit] | OpPacket Version PacketType [Packet] deriving (Show)

parseOperator :: Version -> PacketType -> [Bit] -> (Packet, [Bit])
parseOperator v t (Z : bs) = (OpPacket v t subPackets, rest)
  where
    (subPackets, rest) = parseOperator15 bs
parseOperator v t (I : bs) = (OpPacket v t subPackets, rest)
  where
    (subPackets, rest) = parseOperator11 bs
parseOperator _ _ [] = undefined

parseOperator15 :: [Bit] -> ([Packet], [Bit])
parseOperator15 bs = (subPackets, rest')
  where
    (first15, rest) = splitAt 15 bs
    plen = toInt first15
    (pdata, rest') = splitAt plen rest
    subPackets = parsePackets pdata

parseOperator11 :: [Bit] -> ([Packet], [Bit])
parseOperator11 bs = (subPackets, rest')
  where
    (first11, rest) = splitAt 11 bs
    (subPackets, rest') = parseNPackets (toInt first11) rest

parseNPackets :: Int -> [Bit] -> ([Packet], [Bit])
parseNPackets 0 bs = ([], bs)
parseNPackets x bs =
    let (p, rest) = parsePacket bs
        (ps, rest') = parseNPackets (x - 1) rest
     in (p : ps, rest')

parsePacket :: [Bit] -> (Packet, [Bit])
parsePacket bs =
    let (v, bs') = parseVersion bs
        (pt, bs'') = parseType bs'
     in case pt of
            PData -> parseDataPacket v bs''
            POperator _ -> parseOperator v pt bs''

parsePackets :: [Bit] -> [Packet]
parsePackets bs
    | isNothing (find (== I) bs) = []
    | otherwise =
        let (p, rest) = parsePacket bs
         in p : parsePackets rest

showPacket :: Packet -> String
showPacket (DPacket v d) = "Data: version=" ++ show v ++ ", value=" ++ show (toInt d)
showPacket (OpPacket v t ps) = "Operator: version=" ++ show v ++ ", sub packets:\n" ++ unlines (fmap showPacket ps)

sumVersions :: Packet -> Int
sumVersions (DPacket v _) = v
sumVersions (OpPacket v _ ps) = v + sum (fmap sumVersions ps)

eval :: Packet -> Int
eval (DPacket _ d) = toInt d
eval (OpPacket _ (POperator OpSum) ps) = sum $ fmap eval ps
eval (OpPacket _ (POperator OpProduct) ps) = product $ fmap eval ps
eval (OpPacket _ (POperator OpMin) ps) = minimum $ fmap eval ps
eval (OpPacket _ (POperator OpMax) ps) = maximum $ fmap eval ps
eval (OpPacket _ (POperator OpGT) ps) =
    let (v1 : v2 : _) = take 2 $ fmap eval ps
     in if v1 > v2 then 1 else 0
eval (OpPacket _ (POperator OpLT) ps) =
    let (v1 : v2 : _) = take 2 $ fmap eval ps
     in if v1 < v2 then 1 else 0
eval (OpPacket _ (POperator OpEQ) ps) =
    let (v1 : v2 : _) = take 2 $ fmap eval ps
     in if v1 == v2 then 1 else 0
eval _ = undefined

solve :: String -> String
solve = show . eval . head . parsePackets . hexToBits

main :: IO ()
main = interact solve