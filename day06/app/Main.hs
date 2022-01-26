module Main where

import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepBy)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf (printf)

readInput :: String -> IO [Int]
readInput path = do
  raw <- readFile path
  case parse numbers path raw of
    Left bundle -> error (errorBundlePretty bundle)
    Right xs -> return xs

type Parser = Parsec Void String

numbers :: Parser [Int]
numbers = decimal `sepBy` char ','

day :: [Int] -> [Int]
day [] = []
day (0 : xs) = 6 : 8 : day xs
day (x : xs) = x - 1 : day xs

solution :: [Int] -> Int
solution x = length $ iterate day x !! 80

main :: IO ()
main = do
  readInput "day06/example.txt" >>= printf "Day06: %d\n" . solution
