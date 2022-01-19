module Main where

import Text.Printf (printf)

readInput :: String -> IO [Int]
readInput path = do
  content <- readFile path
  let depths = read <$> lines content
  return depths

solution :: [Int] -> Int
solution [] = 0
solution [x] = 0
solution (x : y : xs) =
  let rest = solution (y : xs)
   in if y > x
        then 1 + rest
        else rest

main = do
  content <- readInput "day01/example.txt"
  printf "Day01: %d\n" $ solution content
