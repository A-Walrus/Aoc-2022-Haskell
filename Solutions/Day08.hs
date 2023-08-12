module Solutions.Day08 (solution) where

import Base
import Data.Char
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Int]]

parse :: String -> Parsed
parse = (map . map) digitToInt . lines

part1 :: Parsed -> Int
part1 g = trues
  where
    bools = runAllDirs visibleDir (\(a, b, c, d) -> a || b || c || d) g
    trues = length $ filter id $ concat bools

part2 :: Parsed -> Int
part2 = maximum . concat . scenicScores

left :: [[a]] -> [[a]]
left = map reverse

left' :: [[a]] -> [[a]]
left' = map reverse

up = transpose

up' = transpose

down :: [[a]] -> [[a]]
down = map reverse . transpose

down' :: [[a]] -> [[a]]
down' = transpose . map reverse

runAllDirs :: ([[a]] -> [[b]]) -> ((b, b, b, b) -> c) -> [[a]] -> [[c]]
runAllDirs f h g = combine
  where
    u = up' $ f $ up g
    r = f g
    d = down' $ f $ down g
    l = left' $ f $ left g
    combine = map (\(a, b, c, d) -> map h $ zip4 a b c d) $ zip4 u d l r

visibleDir :: [[Int]] -> [[Bool]]
visibleDir = map (visibleRow $ -1)

visibleRow :: Int -> [Int] -> [Bool]
visibleRow _ [] = []
visibleRow n (x : xs)
  | x > n = True : visibleRow x xs
  | otherwise = False : visibleRow n xs

scenicScores :: [[Int]] -> [[Int]]
scenicScores g = vals
  where
    vals = runAllDirs scenicDir (\(a, b, c, d) -> a * b * c * d) g

scenicDir :: [[Int]] -> [[Int]]
scenicDir = map (map scenicChunk . init . tails)

scenicChunk :: [Int] -> Int
scenicChunk [x] = 0
scenicChunk (x : xs)
  | good < length xs = good + 1
  | otherwise = good
  where
    good = length (takeWhile (< x) xs)
