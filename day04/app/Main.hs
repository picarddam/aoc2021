module Main where

import Data.List (transpose)
import Data.List.Split (splitOn)
import Text.Printf (printf)

type Grid = [[Int]]

type Pool = [Int]

readInput :: String -> IO (Pool, [Grid])
readInput path = do
  raw <- readFile path
  let l = lines raw
  let pool = read <$> (splitOn "," . head) l
  let gridlines = tail . tail $ l
  let grids = parseGrid <$> splitOn [""] gridlines
  return (pool, grids)

parseGrid :: [String] -> Grid
parseGrid = map (map read . words)

checkLine :: Pool -> [Int] -> Bool
checkLine pool = all (`elem` pool)

checkGrid :: Pool -> Grid -> Bool
checkGrid pool grid =
  let transposed = transpose grid
      lines = any (checkLine pool) grid
      columns = any (checkLine pool) transposed
   in lines || columns

computeScore :: Pool -> Grid -> Int -> Int
computeScore pool grid last =
  let unmarked = filter (`notElem` pool) $ mconcat grid
   in sum unmarked * last

expand :: [a] -> [[a]]
expand = go []
  where
    go l [] = []
    go l (x : xs) = let next = x : l in next : go next xs

solution :: Pool -> [Grid] -> Int
solution numbers grids =
  let winning = head $ filter (\t -> any (checkGrid t) grids) (expand numbers)
      grid = head $ filter (checkGrid winning) grids
   in computeScore winning grid (head winning)

main :: IO ()
main = do
  (pool, grids) <- readInput "day04/example.txt"
  printf "Day04: %d" (solution pool grids)
