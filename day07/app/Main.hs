module Main where

import Data.List (sortBy)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf (printf)

readInput :: String -> IO [Int]
readInput path = readFile path >>= either (error . errorBundlePretty) return . parse numbers path

type Parser = Parsec Void String

numbers :: Parser [Int]
numbers = decimal `sepBy` char ','

moves :: Int -> [Int] -> [Int]
moves p = map (abs . (-) p)

cost :: Int -> [Int] -> Int
cost p = sum . moves p

costs :: [Int] -> [(Int, Int)]
costs input = do
  let x = minimum input
      y = maximum input
  position <- [x .. y]
  return (position, cost position input)

solution :: [Int] -> Int
solution xs =
  let cmp a b = compare (snd a) (snd b)
      sorted = sortBy cmp . costs $ xs
   in fst . head $ sorted

main :: IO ()
main = readInput "day07/example.txt" >>= printf "Day07: %d\n" . solution
