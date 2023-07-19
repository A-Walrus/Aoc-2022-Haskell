{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solutions.Day17 (solution) where

import Base
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [Dir]

parse :: String -> Parsed
parse = cycle . map parseChar . init
  where
    parseChar '<' = L
    parseChar '>' = R

part1 :: Parsed -> Int
part1 dirs = tallest (states !! 2022)
  where
    x = map (uncurry . process) shapes
    states = map fst $ scanl' (flip ($)) (Set.empty, dirs) x

part2 :: Parsed -> Int
part2 = undefined

width = 7

data Dir = L | R deriving (Show)

type State = Shape

-- Positions relative to bottom left edge
type Shape = Set Pos

shapes :: [Shape]
shapes =
  cycle $
    map
      Set.fromList
      [ [(0, 0), (1, 0), (2, 0), (3, 0)],
        [(1, 0), (0, 1), (1, 1), (1, 2), (2, 1)],
        [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
        [(0, 0), (0, 1), (0, 2), (0, 3)],
        [(0, 0), (0, 1), (1, 0), (1, 1)]
      ]

tallest :: State -> Int
tallest = maybe 0 (+1) . Set.lookupMax . Set.map y

move :: Pos -> Shape -> Shape
move p = Set.map (add p)

moveDir :: Dir -> Shape -> Shape
moveDir L = move (-1, 0)
moveDir R = move (1, 0)

drop1 :: Shape -> Shape
drop1 = move (0, -1)

collides :: State -> Shape -> Bool
collides a b = (bottom b < 0) || (left b < 0) || (right b >= width) || not (Set.null (Set.intersection a b))
  where
    bottom = fromJust . Set.lookupMin . Set.map y
    left = fromJust . Set.lookupMin . Set.map x
    right = fromJust . Set.lookupMax . Set.map x

startingPos :: State -> Pos
startingPos s = (2, tallest s + 3)

process :: Shape -> State -> [Dir] -> (State, [Dir])
process shape state = dropFull moved state
  where
    moved = move (2, tallest state + 3) shape

dropFull :: Shape -> State -> [Dir] -> (State, [Dir])
dropFull shape state (dir : dirs)
  | collides state dropped = (Set.union state moved, dirs)
  | otherwise = dropFull dropped state dirs
  where
    moved = moveShape state shape dir
    dropped = drop1 moved

moveShape :: State -> Shape -> Dir -> Shape
moveShape state shape dir = if collides state moved then shape else moved
  where
    moved = moveDir dir shape
