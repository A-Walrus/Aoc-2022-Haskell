{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solutions.Day02 (solution) where

import Base
import Data.List

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [(Shape, Char)]

parse :: String -> Parsed
parse = map parseChars . lines
  where
    parseChars [op, _, you] = (parseOp op, you)
    parseOp 'A' = Rock
    parseOp 'B' = Paper
    parseOp 'C' = Scissors

part1 :: Parsed -> Int
part1 = total . map fix
  where
    fix (a, b) = (a, parseU b)
    parseU 'X' = Rock
    parseU 'Y' = Paper
    parseU 'Z' = Scissors

part2 :: Parsed -> Int
part2 = total . map (solve . fix)
  where
    fix (a, b) = (a, parseU b)
    parseU 'X' = Loss
    parseU 'Y' = Draw
    parseU 'Z' = Win

solve :: (Shape, Result) -> (Shape, Shape)
solve (s, r) = (s, toEnum $ (fromEnum s + fromEnum r) `mod` 3)

total :: [(Shape, Shape)] -> Int
total = sum . map (uncurry score)

data Shape = Rock | Paper | Scissors deriving (Enum, Show)

data Result = Draw | Win | Loss deriving (Enum, Show)

score :: Shape -> Shape -> Int
score a b = shapeScore + resultScore (result b a)
  where
    resultScore Draw = 3
    resultScore Win = 6
    resultScore Loss = 0
    shapeScore = fromEnum b + 1

result :: Shape -> Shape -> Result
result a b = toEnum $ (fromEnum a - fromEnum b) `mod` 3