{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeLenses, (+~), (-~))
import Control.Monad.State (State, execState, modify)
import Text.Printf (printf)

data Submarine = Submarine
  { _depth :: Int,
    _position :: Int
  }
  deriving (Show, Eq)

makeLenses ''Submarine

data Command = Forward | Up | Down
  deriving (Show, Eq)

type Move = (Command, Int)

readInput :: String -> IO [Move]
readInput path = do
  content <- readFile path
  let commands = parseLine <$> lines content
  return commands

parseLine :: String -> Move
parseLine input =
  let w = words input
      c = parseMove . head $ w
      n = read . head . tail $ w
   in (c, n)

parseMove :: String -> Command
parseMove "forward" = Forward
parseMove "up" = Up
parseMove "down" = Down
parseMove _ = error "Unsupported move"

applyMoves :: [Move] -> State Submarine ()
applyMoves moves = do
  mapM_ applyMove moves

applyMove :: Move -> State Submarine ()
applyMove (op, n) = modify $
  case op of
    Down -> depth +~ n
    Up -> depth -~ n
    Forward -> position +~ n

solution :: [Move] -> Submarine
solution moves = execState (applyMoves moves) defaultSubmarine
  where
    defaultSubmarine = Submarine 0 0

main :: IO ()
main = do
  content <- readInput "day02/example.txt"
  printf "Day02: %s\n" $ show (solution content)
