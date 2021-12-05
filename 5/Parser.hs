module Parser (parseInput) where

import Line
import Text.Parsec (ParseError, Parsec (..), char, digit, endOfLine, many, optional, parse, spaces, string, (<|>))

type Parser = Parsec String ()

parseInput :: String -> Either ParseError [Line]
parseInput = parse parseLines "(stdin)"

parseLines :: Parser [Line]
parseLines = do
    many parseLine

parseLine :: Parser Line
parseLine = do
    p1 <- parsePoint
    spaces
    string "->"
    spaces
    p2 <- parsePoint
    optional endOfLine
    pure $ Line p1 p2

parsePoint :: Parser Point
parsePoint = do
    x1 <- many digit
    char ','
    x2 <- many digit
    pure $ Point (read x1) (read x2)
