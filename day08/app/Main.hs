module Main where

import Data.List.Split (splitOn)
import Text.Printf (printf)

type Input = [[String]]

readInput :: String -> IO Input
readInput path = map parseLine . lines <$> readFile path

parseLine :: String -> [String]
parseLine = splitOn " " . tail . dropWhile (/= '|')

solution :: Input -> Int
solution = length . filter (flip elem [2, 3, 4, 7] . length) . mconcat

main :: IO ()
main = readInput "day08/example.txt" >>= printf "Day08: %d\n" . solution
