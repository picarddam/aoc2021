{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeLenses, (^.))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

data Point = Point
  { _x :: Int,
    _y :: Int
  }
  deriving (Show, Ord, Eq)

makeLenses ''Point

data Segment = Segment
  { _start :: Point,
    _end :: Point
  }
  deriving (Show)

makeLenses ''Segment

readInput :: String -> IO [Segment]
readInput path = do
  raw <- readFile path
  let vectors = parseSegment <$> lines raw
  return vectors

parseSegment :: String -> Segment
parseSegment str =
  let [s, e] = splitOn " -> " str
      [sx, sy] = read <$> splitOn "," s
      [ex, ey] = read <$> splitOn "," e
      start = Point sx sy
      end = Point ex ey
   in Segment start end

isHorizontal :: Segment -> Bool
isHorizontal v = v ^. start . x == v ^. end . x

isVertical :: Segment -> Bool
isVertical v = v ^. start . y == v ^. end . y

isLine :: Segment -> Bool
isLine v = isHorizontal v || isVertical v

-- | Either straight lines where points are aligned or diagonal
points :: Segment -> [Point]
points seg =
  let xs = [min (seg ^. start . x) (seg ^. end . x) .. max (seg ^. start . x) (seg ^. end . x)]
      ys = [min (seg ^. start . y) (seg ^. end . y) .. max (seg ^. start . y) (seg ^. end . y)]
   in take (max (length xs) (length ys)) $ zipWith Point (cycle xs) (cycle ys)

countPoints :: [Point] -> Map.Map Point Int
countPoints = go Map.empty
  where
    go m [] = m
    go m (p : ps') =
      let n = Map.insertWith (+) p 1 m
       in go n ps'

solution :: [Segment] -> Int
solution s =
  let ps = concatMap points s
      m = Map.filter (> 1) $ countPoints ps
   in length m

main :: IO ()
main = do
  vectors <- readInput "day05/example.txt"
  printf "Day05: %d\n" $ solution . filter isLine $ vectors
