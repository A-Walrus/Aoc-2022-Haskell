module Solutions.Day09 (solution) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [Dir]

parse :: String -> Parsed
parse = concatMap (parseLine . words) . lines
  where
    parseLine [dir, count] = replicate (read count) (read dir)

part1 :: Parsed -> Int
part1 = solve 1

part2 :: Parsed -> Int
part2 = solve 9

solve :: Int -> Parsed -> Int
solve n = countUnique . (!! n) . iterate followPositions . headPositions

data Dir = R | L | U | D deriving (Read, Show)

headPositions :: [Dir] -> [Pos]
headPositions = scanl move (0, 0)

followPositions :: [Pos] -> [Pos]
followPositions = scanl follow (0, 0)

countUnique :: [Pos] -> Int
countUnique = length . nub

move :: Pos -> Dir -> Pos
move (x, y) U = (x, y + 1)
move (x, y) D = (x, y - 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

-- Calculate new tail pos from given head and tail pos
follow :: Pos -> Pos -> Pos
follow (xT, yT) (xH, yH)
  | x2 && y2 = ((xH + xT) `div` 2, (yH + yT) `div` 2)
  | x2 = ((xH + xT) `div` 2, yH)
  | y2 = (xH, (yH + yT) `div` 2)
  | otherwise = (xT, yT)
  where
    x2 = abs (xH - xT) == 2
    y2 = abs (yH - yT) == 2
