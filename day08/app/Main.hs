module Main where

import Control.Monad (forM_, when)
import Data.List (find, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Text.Printf (printf)

type Input = [([String], [String])]

readInput :: String -> IO Input
readInput path = map parseLine . lines <$> readFile path

parseLine :: String -> ([String], [String])
parseLine s = (input s, output s)
  where
    input = map sort . splitOn " " . init . takeWhile (/= '|')
    output = map sort . splitOn " " . drop 2 . dropWhile (/= '|')

solveInput :: [String] -> [(String, Int)]
solveInput i = zip (map head [zero, one, two, three, four, five, six, seven, eight, nine]) [0 ..]
  where
    zero = [x | x <- i, length x == 6, f <- four, any (`notElem` x) f, x `notElem` six]
    one = [x | x <- i, length x == 2]
    two =
      [ x
        | x <- i,
          length x == 5,
          x `notElem` three,
          x `notElem` five
      ]
    three =
      [ x
        | x <- i,
          length x == 5,
          n <- nine,
          all (`elem` n) x,
          x `notElem` five
      ]
    four = [x | x <- i, length x == 4]
    five = [x | x <- i, length x == 5, s <- six, all (`elem` s) x]
    six = [x | x <- i, length x == 6, o <- one, any (`notElem` x) o]
    seven = [x | x <- i, length x == 3]
    eight = [x | x <- i, length x == 7]
    nine =
      [ x
        | x <- i,
          length x == 6,
          x `notElem` six,
          x `notElem` zero
      ]

part2 :: Input -> Int
part2 = sum . map solve
  where
    solve :: ([String], [String]) -> Int
    solve (i, o) =
      let mapping = solveInput i
       in sum . map (\x -> fromJust $ lookup x mapping) $ o

par1 :: Input -> Int
par1 = length . filter (flip elem [2, 3, 4, 7] . length) . snd . mconcat

main :: IO ()
main = do
  c <- readInput "day08/example.txt"
  printf "Day08p1: %d\n" . par1 $ c
  printf "Day08p2: %d\n" . part2 $ c
