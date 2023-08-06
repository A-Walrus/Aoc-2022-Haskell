module Solutions.Day18 (solution) where

import Base hiding (Pos, add)
import Data.List
import Data.List.Split
import Data.Set qualified as Set

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = Set.Set Pos

parse :: String -> Parsed
parse = Set.fromList . map (tuplify . map read . splitOn ",") . lines

part1 :: Parsed -> Int
part1 set =  Set.fold (\p acc -> acc + 6 - numNeighbors set p) 0 set

part2 :: Parsed -> Int
part2 = undefined

tuplify [x, y, z] = (x, y, z)

type Pos = (Int, Int, Int)

add :: Pos -> Pos -> Pos
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

offsets = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

numNeighbors :: Set.Set Pos -> Pos -> Int
numNeighbors set p = length $ filter (`Set.member` set) $ map (add p) offsets