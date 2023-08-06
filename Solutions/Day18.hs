module Solutions.Day18 (solution) where

import Base hiding (Pos, add)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Set qualified as Set

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = Set.Set Pos

parse :: String -> Parsed
parse = Set.fromList . map (tuplify . map read . splitOn ",") . lines

part1 :: Parsed -> Int
part1 set = Set.fold (\p acc -> acc + 6 - numNeighbors set p) 0 set

part2 :: Parsed -> Int
part2 blocks = length (fst a)
  where
    a = fromJust $ find (\(_, l) -> null l) thing
    face = (Set.findMax blocks, (1, 0, 0))
    thing = iterate layer (Set.fromList [face], [face])
    layer :: (Set.Set Face, [Face]) -> (Set.Set Face, [Face])
    layer (set, faces) = (Set.union set (Set.fromList l), l)
      where
        l = filter (`Set.notMember` set) $ concatMap (spread blocks) faces

tuplify [x, y, z] = (x, y, z)

type Pos = (Int, Int, Int)

type Dir = (Int, Int, Int)

type Face = (Pos, Dir)

add :: Pos -> Pos -> Pos
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

offsets = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

numNeighbors :: Set.Set Pos -> Pos -> Int
numNeighbors set p = length $ filter (`Set.member` set) $ map (add p) offsets

opposite :: Dir -> Dir
opposite (x, y, z) = (-x, -y, -z)

perpendicular :: Dir -> [Dir]
perpendicular d = filter (`notElem` [d, opposite d]) offsets

-- 4
spread :: Set.Set Pos -> Face -> [Face]
spread set (pos, dir) = map f dirs
  where
    dirs = perpendicular dir
    f :: Dir -> Face
    f d
      | add pos diag `Set.member` set = (add pos diag, opposite d)
      | add pos d `Set.member` set = (add pos d, dir)
      | otherwise = (pos, d)
      where
        diag = add d dir
