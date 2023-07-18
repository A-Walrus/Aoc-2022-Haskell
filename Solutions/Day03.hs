module Solutions.Day03 (solution) where

import Base
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Data.Char

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [String]

parse :: String -> Parsed
parse = lines

part1 :: Parsed -> Int
part1 = sum . map (priority . double)

double s = head $ Set.toList intersection
  where
    len = length s
    (a,b) = dMap Set.fromList $ splitAt (len `div` 2) s
    intersection = Set.intersection a b

part2 :: Parsed -> Int
part2 = sum . map (priority . badge) . groups
  where
    groups = chunksOf 3
    badge = head . Set.toList . foldr1 Set.intersection . map Set.fromList


priority :: Char -> Int
priority c = if isUpper c
  then 27 + ord c - ord 'A'
  else 1 + ord c - ord 'a'