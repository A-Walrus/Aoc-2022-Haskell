{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solutions.Day15 (solution) where

import Base
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (fromList)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [Sensor]

parse :: String -> Parsed
parse = map parseLine . lines
  where
    parseLine = fromWords . words
    fromWords [_, _, x1, y1, _, _, _, _, x2, y2] = (dMap . dMap) trimRead ((x1, y1), (x2, y2))
    trimRead = read . filter (\x -> isDigit x || x == '-')

part1 :: Parsed -> Int
part1 sensors = sum (map size merged) - beaconsOnLine
  where
    line = 2000000
    merged = onLinemany sensors line
    beaconsOnLine = length $ fromList $ filter (\(_, y) -> y == line) $ map snd sensors

part2 :: Parsed -> Integer
part2 sensors = toInteger x * toInteger num + toInteger y
  where
    y = fromJust $ find (not . contains (Range 0 (num + 1)) . onLinemany sensors) [0 .. num]
    x = fromJust $ find (\end -> end >= 0 && end < num) $ map end $ onLinemany sensors y

type Sensor = (Pos, Pos)

-- inclusive start, non inclusive end
data Range = Range {start :: Int, end :: Int} deriving (Show)

size :: Range -> Int
size (Range s e) = e - s

distance :: Pos -> Pos -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- distance a = uncurry (+) . dMap abs . uncurry bimap (dMap (-) a)

onLine :: Int -> Sensor -> Maybe Range
onLine line s
  | rem < 0 = Nothing
  | otherwise = Just $ Range (x pos - rem) (x pos + rem + 1)
  where
    pos = fst s
    dist = uncurry distance s
    yDist = abs (line - y pos)
    rem = dist - yDist

onLinemany :: [Sensor] -> Int -> [Range]
onLinemany sensors line = merge sorted
  where
    ranges = mapMaybe (onLine line) sensors
    sorted = sortBy (\a b -> compare (start a) (start b)) ranges

num = 4000000

contains :: Range -> [Range] -> Bool
contains (Range start end) = any (\(Range s e) -> start >= s && end <= e)

merge :: [Range] -> [Range]
merge [] = []
merge [x] = [x]
merge (r@(Range s e) : (next : l))
  | e >= start next = merge (Range s (max e (end next)) : l)
  | otherwise = r : merge (next : l)