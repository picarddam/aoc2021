module Main where

import Data.List (transpose)
import Text.Printf (printf)

readInput :: String -> IO [[Bool]]
readInput path = do
  content <- readFile path
  let bits = readBits <$> lines content
  return bits

readBits :: String -> [Bool]
readBits = map readBit
  where
    readBit '0' = False
    readBit '1' = True
    readBit _ = error "Unexpected value"

mostCommon :: [Bool] -> Bool
mostCommon bools =
  let trues = length . filter id $ bools
   in trues * 2 >= length bools

parseBin :: [Bool] -> Int
parseBin bits = foldl go 0 (map (\b -> if b then 1 else 0) bits) where
  go acc v = acc * 2 + v

solution :: [[Bool]] -> Int
solution input =
  -- Transpose input to fold based on column position instead of row
  let transposed = transpose input
      -- Deduce gamma from most common bytes at positions
      gamma = map mostCommon transposed
      -- Deduce gamma from least common bytes at positions
      -- Least common defined as not the most common
      epsilon = map not gamma
  in parseBin gamma * parseBin epsilon

main :: IO ()
main = do
  content <- readInput "day03/example.txt"
  printf "Day03: %d\n" $ solution content
