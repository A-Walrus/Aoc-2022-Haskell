{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solutions.Day04 (solution) where

import Base
import Data.List
import Data.List.Split

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [(Sections, Sections)]

type Sections = (Int, Int)

parse :: String -> Parsed
parse = map line . lines
  where
    splits = map (map read . splitOn "-") . splitOn "," 
    line = tuple . map tuple . splits
    tuple [a, b] = (a, b)

part1 :: Parsed -> Int
part1 = length . filter (uncurry contains)

part2 :: Parsed -> Int
part2 = length . filter (uncurry overlaps)

contains :: Sections -> Sections -> Bool
contains (a1, a2) (b1, b2)
  | a1 == b1 = True
  | a1 > b1 = contains (b1, b2) (a1, a2)
  | otherwise = a2 >= b2

overlaps :: Sections -> Sections -> Bool
overlaps (a1, a2) (b1, b2)
  | a1 == b1 = True
  | a1 > b1 = overlaps (b1, b2) (a1, a2)
  | otherwise = a2 >= b1
