module Solutions.Day01 (solution) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Int]]

parse :: String -> Parsed
parse = foldr (\a l@(x : xs) -> if a == "" then [] : l else (read a : x) : xs) [[]] . lines

part1 :: Parsed -> Int
part1 = maximum . map sum

part2 :: Parsed -> Int
part2 = sum . take 3 . sortBy (flip compare) . map sum